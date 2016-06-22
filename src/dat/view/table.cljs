(ns dat.view.table
  (:require-macros [reagent.ratom :refer [reaction]]
                   [cljs.core.async.macros :refer [go go-loop]])
  ;; Need to abstract the ws
  (:require [dat.view :as dat.view]
            [dat.view.router :as router]
            [dat.view.settings :as settings]
            [cljs-time.core :as cljs-time]
            [cljs-time.format]
            [cljs-time.coerce]
            [cljs.pprint :as pp]
            [cljs.core.match :as match :refer-macros [match]]
            [cljs.core.async :as async]
            [datascript.core :as d]
            [reagent.core :as r]
            [re-com.core :as re-com]
            [posh.core :as posh]
            [testdouble.cljs.csv :as csv]
            ;; Couldn't get this to work, but would be nice to try again
            ;[cljsjs.papaparse :as csv]
            [cljs-uuid-utils.core :as uuid]
            [goog.date.Date]
            [bidi.bidi :as bidi]))

;; This namespace has to be completely rewritten in terms of the new datview; Currently in terms of old.
;; Though this was actually part of the inspiration for old so hopefully it won't be too bad...


;; ## Table views

;; In terms of the data flow, this may be the best written part of the application so far.
;; The performance here is pretty darn snappy, and it is achieved by really agressively limiting the number of
;; queries that need to be run to render.
;; I think this is the brilliance of om-next in emphasizing pull expressions; they're more composable towards
;; this end
;; Anyway...

;; If we can do something similar with the "base" datview namespace, I think we'll be able to really tackle
;; the general performance problems...


;; ### The model, effectively

;; As datomic schema, would look roughly like this: But need a better way to transact this stuff in datsync I
;; think, since we might not want to actually mark it with remote.db/id

;(def table-column-selector-schema
  ;[{:db/id -1
    ;:db/ident :table.view.column/attributes
    ;:db/cardinality [:db/ident :db.cardinality/many]
    ;:db/valueType [:db/ident :db.type/ref]}
   ;{:db/id -2
    ;:db/ident :table.view.column.selector/unfolded-types
    ;:db/cardinality [:db/ident :db.cardinality/many]
    ;:db/valueType [:db/ident :db.type/ref]}
   ;{:db/id -3
    ;:db/ident :table.view/column-selector}])

;; Here's what this looks like as datascript schema:

(def table-column-selector-schema
  {:table.view.column/attributes {:db/cardinality :db.cardinality/many
                                  :db/valueType :db.type/ref}
   :table.view.column.selector/unfolded-types {:db/cardinality :db.cardinality/many
                                               :db/valueType :db.type/ref}})


;; We store our data around a column selector entity

;; I maybe should have been doing this as boolean meta-attributes. That would have been a bit easier...
;; This solution however makes it possible to have multiple such idents
;; (or other entities) if we want to have multiple sets of saved attributes.
;; esaier if we want to provide a few different options, so here it is.

(defn install-table-view-column-selector!
  "Install the :table.view/column-selector ident locally, so we can manipulate it's
  :table.view.column.selector/unfolded-types and :table.view.column/attributes attributes"
  [app]
  (d/transact! (:conn app) [{:db/id -3
                             :db/ident :table.view/column-selector}]))

(def column-selector
  "Simple accessor for the column-selector entity"
  [:db/ident :table.view/column-selector])

(defn selected-columns
  [app]
  (posh/pull (:conn app) '[:table.view.column/attributes] column-selector))

(def type-tree-pull-pattern
  '[:db/id :e/type :db/ident :e/name :table.view.column.selector/_unfolded-types :e.type/_isa
    {:e.type/attributes [:db/id :db/ident :attribute/label :table.view.column/_attributes :attribute.ref/types]}])

(defn type-attribute-tree
  "Returns a posh reaction of the recursive pull of types defined by type-tree-pull-pattern"
  [db type-id]
  (d/pull db type-tree-pull-pattern type-id))

(defn type-attribute-tree-reaction
  "Returns a posh reaction of the recursive pull of types defined by type-tree-pull-pattern"
  [app type-id]
  (posh/pull (:conn app) type-tree-pull-pattern type-id))

(defn selected-column-paths
  ([app base-type-id]
   (selected-column-paths app base-type-id []))
  ([app base-type-id base-path]
   (reaction
     (let [type-entity @(type-attribute-tree-reaction app base-type-id)
           selected-attributes (filter :table.view.column/_attributes (:e.type/attributes type-entity))]))))

;; OK; time to actually construct our table data query.

;; This is going to be recursive.
;; For each type level, we should compute the query needed to get all of the immediately selected attributes.
;; We then recur over the types pointed to by any selected ref attributes
;; As we recur down these types, we hand the recursion the base binding for the entity that it should be bound
;; to.
;; The variables we wish to fetch our also kept in a mapping of paths to variable names (or vice versa) so
;; that we can name columns based on their paths
;; We can't go down each branch separately, but have to in series and reduce into an acculative product.

;; This naming system isn't ideal; it leaves room for duplicates. Should come up with something guaranteed to
;; come up with unique entries
(defn gen-sym
  [attribute-ident]
  (symbol (str "?" (name attribute-ident) "-" (rand-int 10000000))))


(defn unfolded-type?
  ([type-entity]
   (boolean (seq (:table.view.column.selector/_unfolded-types type-entity)))))
;; Should add a second arity to this so it can return a reaction based on ident or eid

(defn selected-attribute?
  ([attr-entity]
   (boolean (:table.view.column/_attributes attr-entity))))

(declare apply-type-to-query)

(defn apply-attribute-to-query
  [db {:as context :keys [query base-path sym-mapping base-sym]} attr-entity]
  (let [attr-ident (:db/ident attr-entity)
        attr-sym (gen-sym attr-ident)
        path (conj base-path attr-ident)
        sym-mapping (assoc sym-mapping attr-sym path)
        new-context (assoc context
                           :query (-> query
                                      (update-in [:find] conj attr-sym)
                                      (update-in [:where] conj [base-sym attr-ident attr-sym]))
                           :base-path path
                           :sym-mapping sym-mapping
                           :base-sym attr-sym)]
    (if-let [attr-ref-types (seq (:attribute.ref/types attr-entity))]
      (reduce
        (fn [current-context type-id]
          (apply-type-to-query db new-context type-id))
        new-context
        (map :db/id attr-ref-types))
      new-context)))

(defn apply-type-to-query
  ([db base-type-id]
   (apply-type-to-query db {:base-path [] :sym-mapping {}} base-type-id))
  ([db {:as context :keys [query base-path sym-mapping base-sym]} base-type-id]
   (let [type-entity (type-attribute-tree db base-type-id)
         base-sym (or base-sym '?eid)
         query (or query {:find [base-sym] :in '[$ [?eid ...]] :where '[[?eid]]})]
     (reduce
       (fn [current-context attr-entity]
         (assoc (apply-attribute-to-query db current-context attr-entity)
                :base-path base-path
                :base-sym base-sym))
       (assoc context :query query :base-sym base-sym)
       (filter selected-attribute? (:e.type/attributes type-entity))))))

(defn type-query
  ([db base-type-id]
   (:query (apply-type-to-query db base-type-id))))

(defn type-query-reaction
  ([conn-reaction base-type-id]
   (reaction (dissoc (apply-type-to-query @conn-reaction base-type-id) :base-path :base-sym))))

(defn r-unfolded-type?
  [app type-id]
  (reaction (unfolded-type? @(posh/pull (:conn app) '[:table.view.column.selector/_unfolded-types] type-id))))

(defn collapse-button
  "A collapse button for hiding information; arg collapse? should be a bool or an ratom thereof.
  If no click handler is specified, toggles the atom."
  ([collapse? on-click-fn]
   (let [[icon-name tooltip] (if (try @collapse? (catch js/Object e collapse?)) ;; not positive this will work the way I expect
                               ["zmdi-caret-right" "Expand collection"]
                               ["zmdi-caret-down" "Hide collection"])]
     [re-com/md-icon-button :md-icon-name icon-name
                            :tooltip tooltip
                            :on-click on-click-fn]))
  ([collapse?]
   (collapse-button collapse? (fn [] (swap! collapse? not)))))



(defn type-folder
  [app type-entity]
  ;; Could do this as pull, but would only want to if we had hooked up smarter query rendering capabilities
  (let [type-id (:db/id type-entity)
        unfolded? (r-unfolded-type? app type-id)]
    (fn [_ type-entity]
      [re-com/h-box
       :gap "3px"
       :children [[dat.view/collapse-button
                   @unfolded?
                   (fn [] (d/transact! (:conn app)
                                       [[(if @unfolded? :db/retract :db/add)
                                         [:db/ident :table.view/column-selector]
                                         :table.view.column.selector/unfolded-types
                                         type-id]]))]
                  [re-com/label :style {:font-weight "bold"} :label (:e/name type-entity)]]])))

;; What about a compilation step for components to pull out from them all the logic of changing the schema,
;; since that should only happen infrequently? XXX

(declare attribute-column-selector-rows)

(defn attribute-column-selector-row
  "The selector for whether a particular attribute should end up in the output data. Seen should be a set of type identities which
  have already been seen to avoid infinite recursion with type reference cycles (overcoats)."
  ([app attr-entity seen]
   (let [conn (:conn app)
         checked? (posh/pull conn '[:table.view.column/_attributes] (:db/id attr-entity))]
     [re-com/v-box
      :style {:padding-left "12px"}
      :gap "3px"
      :children [[re-com/h-box
                  :gap "5px"
                  :children [[re-com/checkbox
                              :model checked?
                              :on-change (fn [checked-now?]
                                           (d/transact! conn [[(if checked-now? :db/add :db/retract) [:db/ident :table.view/column-selector] :table.view.column/attributes (:db/id attr-entity)]]))]
                             [re-com/label :label (dat.view/pull-summary attr-entity)]]]
                 (when @checked?
                   ;; Present types for possible expansion
                   [re-com/v-box
                    :children (for [ref-type-id (map :db/id (:attribute.ref/types attr-entity))]
                                ;; Not sure what the semantics of nil/key are in this case?
                                ^{:key ref-type-id}
                                [attribute-column-selector-rows app ref-type-id seen])])]])))

(defn attribute-column-selector-rows
  "The rows in a attribute column selector for a table view given a type entity view with nested
  entities for attributes and subtypes."
  ([app type-id]
   (attribute-column-selector-rows app type-id #{}))
  ([app type-id seen]
   (let [type-entity (type-attribute-tree-reaction app type-id)
         unfolded? (r-unfolded-type? app type-id)]
     (fn [_ type-id]
       (let [type-eid (:db/id @type-entity)]
         (if-not (seen type-eid)
           [re-com/v-box
            :style {:padding-left "10px"}
            :children [[type-folder app @type-entity]
                       (when (not @unfolded?)
                         [re-com/v-box
                          ;; First, render the same thing for the subtypes, so their attributes can show up if unfolded
                          :children [[re-com/v-box
                                      :children (for [subtype-id (map :db/id (:e.type/_isa @type-entity))]
                                                  ^{:key subtype-id}
                                                  [attribute-column-selector-rows app subtype-id (conj seen type-eid)])]
                                     ;; This is all of the types directly assigned attribute selection rows
                                     [re-com/v-box
                                      :children (for [attr-entity (:e.type/attributes @type-entity)]
                                                  ^{:key (:db/id attr-entity)}
                                                  [attribute-column-selector-row app attr-entity (conj seen type-eid)])]]])]]
           [re-com/label :label "Seen"]))))))

;; Build a magical selector for attributes
(defn attribute-column-selector
  "The top level attribute column selector component; based on type eid or lookup ref (like [:db/ident :e.type/Comment])."
  [app base-type]
  (let [collapse? (r/atom true)]
    (fn [_ base-type]
      [re-com/v-box
       :children [[re-com/h-box
                   :children [[dat.view/collapse-button collapse?]
                              [re-com/title :level :level3 :label "Table column selector:"]]]
                  (when-not @collapse?
                    [re-com/border
                     :border "1px solid black"
                     :width "300px"
                     :max-height "300px"
                     :style {:overflow-y "scroll"}
                     :child [attribute-column-selector-rows app (:db/id @(posh/pull (:conn app) '[:db/id] base-type))]])]])))

(defn entity-row-view
  [paths row]
  [:tr
   (for [[path value] (map vector paths row)]
     ^{:key (hash path)}
     [:td
      {:style {:padding "4px 8px"}}
      value])])


(defn path-name
  [path]
  (if-let [path-names (seq (map name path))]
    (clojure.string/join "/" path-names)
    "base-eid"))

(defn ordered-paths
  [{:as query-context :keys [query sym-mapping]}]
  (mapv sym-mapping (:find query)))

(defn header-view
  [{:as query-context :keys [query sym-mapping]}]
  (let [find-syms (:find query)]
    [:tr
     (for [sym find-syms]
       (let [path (sym-mapping sym)]
         ^{:key (hash path)}
         [:th
          {:style {:padding "8px"}}
          (path-name path)]))]))


;; Writing out results

;(defn evaluate-query
  ;[app query-context eids]
  ;(posh/q (:conn app) (:query query-context) eids))

;; Here's more or less how you can do the download in js (from
;; http://stackoverflow.com/questions/14964035/how-to-export-javascript-array-info-to-csv-on-client-side)
;;
;;    var csvContent = "data:text/csv;charset=utf-8,";
;;    data.forEach(function(infoArray, index){})
;;       dataString = infoArray.join(",");
;;       csvContent += index < data.length ? dataString+ "\n" : dataString;
;;    }); 
;;    
;; Then you can use JavaScript's window.open and encodeURI functions to download the CSV file like so:
;;    
;;    var encodedUri = encodeURI(csvContent);
;;    window.open(encodedUri);


(defn format-csv
  [rows]
  (->> rows
       (map (partial map pr-str))
       (csv/write-csv)))

;; XXX Would really like to use papa for this but can't figure out how
;(println (format-csv [["1,000" "2" "3" "frank,this"] [4 "5,\"000" "6"]]))

(defn download-csv
  [paths rows]
  (let [csv-content "data:text/csv;charset=utf-8,"
        rows (vec (concat [(mapv path-name paths)]
                          rows))
        csv-content (str csv-content (format-csv rows))
        encode-uri (js/encodeURI csv-content)]
    (.open js/window encode-uri)))

(defn table-view
  [app eids base-type]
  (let [conn (:conn app)
        conn-reaction (dat.view/as-reaction conn)
        query-context (type-query-reaction conn-reaction base-type)]
        ;query-results (evaluate-query conn query-context eids)]
    (fn [_ eids]
      (let [ordered-paths (ordered-paths @query-context)
            rows @(posh/q conn (:query @query-context) eids)] 
        [re-com/v-box
         :gap "15px"
         :children [[re-com/title :level :level2 :label "Table view"]
                    [re-com/h-box
                     :gap "20px"
                     :children [[re-com/md-icon-button
                                 :md-icon-name "zmdi-download"
                                 :tooltip "Download table as CSV"
                                 :on-click (partial download-csv ordered-paths rows)]
                                [attribute-column-selector app base-type]]]
                    [:table
                     [:tbody
                      [header-view @query-context]
                      (for [row rows]
                        ^{:key (hash row)}
                        [entity-row-view ordered-paths row])]]]]))))


