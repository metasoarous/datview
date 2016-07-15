(ns dat.view.query)

(def rules
  '[[(attr-ident-value-type-ident ?attr-ident ?value-type-ident)
     [?attr :db/ident ?attr-ident]
     [?attr :db/valueType ?value-type]
     [?value-type :db/ident ?value-type-ident]]])
    ;[(type-attr ?type ?attr)]])


