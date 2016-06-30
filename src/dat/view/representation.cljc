(ns dat.view.representation
  (:require
    #?(:cljs [reagent.core :as r])
    #?(:cljs [reagent.ratom :refer-macros [reaction]])
    [taoensso.timbre :as log]))


(defmulti represent*
  "Reprsent some data given a context"
  (fn [app [context-id context-data] data]
    context-id))


(defn represent
  [app context data]
  [represent* app context data])


;; TODO Replace evil global mutable state with local values!
(def registrations
  (#?(:cljs r/atom :clj atom) {}))

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

(defn register-representation
  "Registers a representation function under the given context-id, given middleware (to which reactively-register is appended)."
  ([context-id middleware representation-fn]
   (let [base-middleware [(partial reactively-register context-id)]
         middleware (concat middleware base-middleware)
         middleware-fn (apply comp middleware)
         representation-fn' (middleware-fn representation-fn)]
     (swap! registrations assoc context-id representation-fn')
     (defmethod represent* context-id
       [app context data]
       (representation-fn' app context data))))
  ([context-id representation-fn]
   (register-representation context-id [] representation-fn)))




