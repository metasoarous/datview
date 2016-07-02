(ns dat.view.representation
  (:require
    #?(:cljs [reagent.core :as r])
    #?(:cljs [reagent.ratom :refer-macros [reaction]])
    [taoensso.timbre :as log]))


(defn cljc-atom [init-value]
  (#?(:cljs r/atom :clj atom)
    init-value))

;(def represent* nil)
(defmulti represent*
  "Reprsent some data given a context"
  (fn [_ context _]
    (try
      (first context)
      (catch #?(:clj Exception :cljs :default) e
        (log/error "Could not dispatch on malformed context:" context)))))


(defn represent
  [app context data]
  #?(:cljs (r/create-class
             {:display-name (str "representation " (try (first context) (catch :default _ "?")))
              :reagent-render
              (fn [app context data]
                [represent* app context data])})
     :clj [represent* app context data]))


;; TODO Replace evil global mutable state with local values!
(def registrations
  (cljc-atom {}))

(defn reactively-register
  "Representation middleware: *Should* make it so that when we update representations on the client, they update in the views."
  [context-id representation-fn]
  (swap! registrations assoc context-id representation-fn)
  (let [registration-reaction #?(:cljs (reaction (get @registrations context-id))
                                 :clj registrations)]
    (fn [app context data]
      ;; Goal: This should only update if we have changed the representation (in cljs); We'll see :-)
      ;; If we defined reaction in clj, we could actually _use_ the defer value to compute the new function
      @registration-reaction
      (representation-fn app context data))))

(defn handle-errors
  "Representation middleware: *Should* make it so that when we update representations on the client, they update in the views."
  [context-id representation-fn]
  (fn [app context data]
    ;; Goal: This should only update if we have changed the representation (in cljs); We'll see :-)
    ;; If we defined reaction in clj, we could actually _use_ the defer value to compute the new function
    (try
      (representation-fn app context data)
      (catch #?(:clj Exception :cljs :default) e
        (let [collapse? (cljc-atom true)]
          (fn [app context data]
            (log/error e (str "Exception raised for representation: " context-id))
            [:div.error {:style {:border-style "solid" :border-color "red" :padding "8px 12px" :margin "15px 3px"}}
             [:p [:strong "Error rendering component " (str context-id)]]
             [:p [:a {:on-click (fn [& args] (swap! collapse? not))} "See more/less"]]
             (when-not @collapse?
               [:div
                [:p "Error"]
                [:pre e
                 #?(:cljs (.-stack e))]
                [:p "Context:"]
                [:pre context]
                [:p "Data:"]
                [:pre data]])]))))))



(defn register-representation
  "Registers a representation function under the given context-id, given middleware (to which reactively-register is appended)."
  ([context-id middleware representation-fn]
   (let [base-middleware [(partial reactively-register context-id)
                          (partial handle-errors context-id)]
         middleware (concat middleware base-middleware)
         middleware-fn (apply comp middleware)
         representation-fn' (middleware-fn representation-fn)]
     (swap! registrations assoc context-id representation-fn')
     (defmethod represent* context-id
       [app context data]
       (representation-fn' app context data))))
  ([context-id representation-fn]
   (register-representation context-id [] representation-fn)))




