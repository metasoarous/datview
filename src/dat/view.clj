(ns dat.view
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [datomic.api]))

(def base-schema
  (->> "datview-schema.edn"
       ;; Swap out once a lib XXX TODO
       io/resource
       slurp
       (edn/read-string {:readers *data-readers*})))

;; Other things for the clj version?
      
