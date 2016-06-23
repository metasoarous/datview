(ns dat.view
  (:require [clojure.java.io :as io]))


(def base-schema
  (->> "datview-schema.edn"
       ;; Swap out once a lib XXX TODO
       io/resource
       slurp
       read-string))

;; Other things for the clj version?
      
