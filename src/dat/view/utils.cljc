(ns dat.view.utils)

(defn deref-or-value
  [val-or-atom]
  (if (satisfies? #?(:cljs IDeref :clj clojure.lang.IDeref) val-or-atom) @val-or-atom val-or-atom))

(defn deep-merge
  "Like merge, but merges maps recursively."
  ;; XXX Need to be able to specify customizations for how things like vectors are going to merge;
  ;; May not always want to do what is described here
  [& maps]
  (if (every? #(or (map? %) (nil? %)) maps)
    (apply merge-with deep-merge maps)
    (last maps)))

