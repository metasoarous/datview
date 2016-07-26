(ns dat.view.utils
  ;(:require-macros [reagent.ratom :refer [reaction]])
  (:require
    [datascript.core :as d]
    [reagent.core :as r]
    [reagent.ratom :as ratom :refer-macros [reaction]]
    [posh.reagent :as posh]))

;(defn deref-or-value
;  [val-or-atom]
;  (if (satisfies? #?(:cljs IDeref :clj clojure.lang.IDeref) val-or-atom) @val-or-atom val-or-atom))
(defn deref-or-value
  [val-or-atom]
  (if (satisfies? IDeref val-or-atom) @val-or-atom val-or-atom))

(defn deep-merge
  "Like merge, but merges maps recursively."
  ;; XXX Need to be able to specify customizations for how things like vectors are going to merge;
  ;; May not always want to do what is described here
  [& maps]
  (if (every? #(or (map? %) (nil? %)) maps)
    (apply merge-with deep-merge maps)
    (last maps)))


(def ratom r/atom)

;#?(:clj
;   (defmacro fake-reaction
;     [& forms]
;     `()))


(defn persistent-reaction
  [rx-fn]
  (let [last-value (atom nil)]
    (ratom/make-reaction
      (fn [& args]
        (let [result (apply rx-fn args)]
          (reset! last-value result)
          result)))))


   ;; If we build/mock a reaction macro for clj, we could build these out for both
(defn as-reaction
  "Treat a regular atom as though it were a reaction"
  [vanilla-atom]
  (let [trigger (ratom 0)]
    (add-watch vanilla-atom :as-reaction-trigger (fn [& _] (swap! trigger inc)))
    (reaction
      @trigger
      @vanilla-atom)))


;; XXX This will be coming to posh soon, but in case we need it earlier

(def safe-pull
  "A version of posh/pull where missing lookup refs should behave properly (generally), but also behave more like Datomic than
  DataScript by returning `{:db/id nill}` instead of erroring when passed bad lookup refs."
  ;(memoize
  (fn safe-pull*
    [conn pattern eid-or-lookup]
    ;(log/debug "eid-or-lookup" eid-or-lookup)
    (cond
      ;; If integer
      (integer? eid-or-lookup)
      (posh/pull conn pattern eid-or-lookup)
      ;; Make sure not nil...
      (nil? eid-or-lookup)
      (reaction {:db/id nil})
      ;; TODO Hmm... should we be testing here to make sure this is unique and actually a lookup ref
      (vector? eid-or-lookup)
      (let [eid-rx (posh/q [:find '?e '. :where (into '[?e] eid-or-lookup)] conn)]
        (reaction
          (let [eid @eid-rx]
            (if (integer? eid)
              @(posh/pull conn pattern eid)
              {:db/id nil})))))))

(def safe-q
  "A version of posh/q without any transaction pattern matching filters (al a posh) that delegates directly to d/q, and
  wraps in a reaction"
  (memoize
    (fn [query conn & args]
      (reaction
        (let [conn (as-reaction conn)
              db (deref-or-value conn)
              args (mapv deref-or-value args)]
          (apply d/q query db args))))))

;; QUESTION Should this be wrapped in a reaction as well?
(defn pull-many
  [app pattern eids]
  (map (partial safe-pull (:conn app) pattern)
       (deref-or-value eids)))


(def pull-attr
  "Wraps safe pull, and etracts the results for a given attr"
  (memoize
    (fn [conn eid attr-ident]
      (reaction
        (get @(safe-pull conn [attr-ident] eid) attr-ident)))))


(def pull-path
  ;(memoize
  (fn [conn eid attr-path]
    ;; Question: Should use cursor?
    (reaction
      (get-in
        @(safe-pull conn (vec (filter keyword? attr-path)) eid)
        attr-path))))



