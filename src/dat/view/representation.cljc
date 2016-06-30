(ns dat.view.representation
  (:require
    #?(:clj [reagent.core :as r])
    [taoensso.timbre :as log]))


(defmulti represent*
  "Reprsent some data given a context"
  (fn [app [context-id context-data] data]
    context-id))


(defn represent
  [app context data]
  [represent* app context data])


;; Evil global mutable state!
(def registrations
  (#?(:cljs r/atom :clj atom) {}))


(defn register-representation
  ([context-id middleware representation-fn]
   (let [middleware-fn (apply comp middleware)
         representation-fn' (middleware-fn representation-fn)]
     (swap! registrations assoc context-id representation-fn')
     (defmethod represent* context-id
       [app context data]
       (log/debug "Trying to deref")
       (representation-fn' app context data))))
  ([context-id representation-fn]
   (register-representation context-id [] representation-fn)))


