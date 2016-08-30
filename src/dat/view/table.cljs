(ns dat.view.table
  (:require-macros [reagent.ratom :refer [reaction]]
                   [cljs.core.async.macros :refer [go go-loop]])
  ;; Need to abstract the ws
  (:require
    [dat.view :as dat.view]
    [dat.view.router :as router]
    [dat.view.settings :as settings]
    [dat.view.representation :as representation]
    [cljs-time.core :as cljs-time]
    [cljs-time.format]
    [cljs-time.coerce]
    [cljs.pprint :as pp]
    [cljs.core.match :as match :refer-macros [match]]
    [cljs.core.async :as async]
    [datascript.core :as d]
    [reagent.core :as r]
    [re-com.core :as re-com]
    [posh.reagent :as posh]
    [testdouble.cljs.csv :as csv]
    ;; Couldn't get this to work, but would be nice to try again
    ;[cljsjs.papaparse :as csv]
    [cljs-uuid-utils.core :as uuid]
    [goog.date.Date]
    [bidi.bidi :as bidi]
    [taoensso.timbre :as log]
    [datafrisk.core :as frisk]))



;; ## The model, effectively

;; Really, we should be doing this by serializing pull expressions as isComponent entities.
;; That'll be more correct and extensible.
;; But I don't have time now to get that right.

;(def pull-expr-schema
;  {:dat.type/PullExpression {:e/type :e.type/Type}
;   :dat.sys/pull {:db/valueType :db.type/ref}
;   :dat.sys.pull/attributes {:db/valueType :db.type/ref}
;   ;; This just points to other pull expressions
;   :dat.sys.pull/ref-attributes {:db/valueType :db.type/ref}})


;; For now,m we'll just be tracking what attributes have been selected or not, and it will recursively pull from all of those.

;; As datomic schema, would look roughly like this: But need a better way to transact this stuff in datsync I
;; think, since we might not want to actually mark it with remote.db/id

;(def table-column-selector-schema
  ;[{:db/id -1
    ;:db/ident ::columns
    ;:db/cardinality [:db/ident :db.cardinality/many]
    ;:db/valueType [:db/ident :db.type/ref]}
   ;{:db/id -2
    ;:db/ident ::types
    ;:db/cardinality [:db/ident :db.cardinality/many]
    ;:db/valueType [:db/ident :db.type/ref]}

;; Here's what this looks like as datascript schema:

(def table-column-selector-schema
  {::columns {:db/cardinality :db.cardinality/many
              :db/valueType :db.type/ref}
   ::base-type {:db/valueType :db.type/ref}
   ::types {:db/cardinality :db.cardinality/many
            :db/valueType :db.type/ref}})

(defn selected-columns
  [app column-selector]
  (posh/pull (:conn app) '[::columns] column-selector))


;; This stuff is for representing the type relation tree, on which we organize the checkboxes for which attributes are selected in the column selector.
;; Really, this should be merged with the idea of serialized (as entities/datoms) pull expressions as the more standard way of doing things.

(def type-tree-pull-pattern
  '[:db/id :e/type :db/ident :e/name ::_types :e.type/_isa
    {:e.type/attributes [:db/id :db/ident :attribute/label ::_columns :attribute.ref/types]}])

(defn type-attribute-tree
  "Returns a posh reaction of the recursive pull of types defined by type-tree-pull-pattern"
  [db type-id]
  (d/pull db type-tree-pull-pattern type-id))

(defn type-attribute-tree-reaction
  "Returns a posh reaction of the recursive pull of types defined by type-tree-pull-pattern"
  [app type-id]
  (posh/pull (:conn app) type-tree-pull-pattern type-id))

;; Not sure what the deal is with this; Must have written and then figured I didn't need it
;(defn selected-column-paths
;  ([app base-type-id]
;   (selected-column-paths app base-type-id []))
;  ([app base-type-id base-path]
;   (reaction
;     (let [type-entity @(type-attribute-tree-reaction app base-type-id)
;           selected-attributes (filter :table.view.column/_attributes (:e.type/attributes type-entity))]))))

;; OK; time to actually construct our table data query.

;; This is going to be recursive.
;; For each type level, we should compute the query needed to get all of the immediately selected attributes.
;; We then recur over the types pointed to by any selected ref attributes.
;; As we recur down these types, we hand the recursion the base binding for the entity that it should be bound
;; to.
;; The variables we wish to fetch our also kept in a mapping of paths to variable names (or vice versa) so
;; that we can name columns based on their paths.
;; We can't go down each branch separately, but have to in series and reduce into an accumulative product.

;; Again, this naming system isn't ideal; it leaves room for duplicates.
;; Ideally we be able to have unique entries, via pull serialization, or some such.


;; Going to eventually reimplement in terms of pull (at least as an option), but for right now, we iteratively build a table

(defn gen-sym
  [attribute-ident]
  (symbol (str "?" (name attribute-ident) "-" (rand-int 10000000))))


;; First some stuff for producing information based on pull data

;; Should add a second arity to this so it can return a reaction based on ident or eid

(defn selected-attribute?
  ([attr-pull-data]
   (boolean (::_columns attr-pull-data))))


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

(defn unfolded-types
  [app column-selector]
  (posh/pull (:conn app) '[* {::types [:db/id]}] column-selector {:cache :forever}))

(defn r-unfolded-type?
  [app column-selector type-id]
  (reaction
    (let [pull-results @(unfolded-types app column-selector)]
      (not
        ((->> pull-results ::types (map :db/id) set)
         type-id)))))

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
  [app column-selector type-entity]
  ;; Could do this as pull, but would only want to if we had hooked up smarter query rendering capabilities
  (let [type-id (:db/id type-entity)
        unfolded? (r-unfolded-type? app column-selector type-id)]
    (fn [app column-selector type-entity]
      [re-com/h-box
       :gap "3px"
       :children [[dat.view/collapse-button
                   @unfolded?
                   ;; TODO Rewrite in terms of the dispatch
                   (fn [] (d/transact! (:conn app)
                                       [[(if @unfolded? :db/add :db/retract)
                                         column-selector
                                         ::types
                                         type-id]]))]
                  [re-com/label :style {:font-weight "bold"} :label (:e/name type-entity)]]])))

;; What about a compilation step for components to pull out from them all the logic of changing the schema,
;; since that should only happen infrequently? XXX

(declare attribute-column-selector-rows)

;(defn selected-columns
;  [app column-selector]
;  (posh/pull (:conn app) [::columns] column-selector))

(defn column-selected?
  [app column-selector attr-eid]
  ;; TODO need to generalize attr-eid here to be any id; ok because only usage below is an entity always
  (reaction
    ((->> @(selected-columns app column-selector) ::columns (map :db/id) set)
     attr-eid)))

(defn attribute-column-selector-row
  "The selector for whether a particular attribute should end up in the output data. Seen should be a set of type identities which
  have already been seen to avoid infinite recursion with type reference cycles (overcoats)."
  ([app column-selector attr-entity seen]
   (let [conn (:conn app)
         checked? (column-selected? app column-selector (:db/id attr-entity))]
     [re-com/v-box
      :style {:padding-left "12px"}
      :gap "3px"
      :children [[re-com/h-box
                  :gap "5px"
                  :children [[re-com/checkbox
                              :model checked?
                              :on-change (fn [checked-now?]
                                           (d/transact! conn [[(if checked-now? :db/add :db/retract) column-selector ::columns (:db/id attr-entity)]]))]
                             [re-com/label :label (dat.view/pull-summary-string attr-entity)]]]
                 (when @checked?
                   ;; Present types for possible expansion
                   [re-com/v-box
                    :children (for [ref-type-id (map :db/id (:attribute.ref/types attr-entity))]
                                ;; Not sure what the semantics of nil/key are in this case?
                                ^{:key ref-type-id}
                                [attribute-column-selector-rows app column-selector ref-type-id seen])])]])))

(defn attribute-column-selector-rows
  "The rows in a attribute column selector for a table view given a type entity view with nested
  entities for attributes and subtypes."
  ([app column-selector type-id]
   [attribute-column-selector-rows app column-selector type-id #{}])
  ([app column-selector type-id seen]
   (let [type-entity (type-attribute-tree-reaction app type-id)
         unfolded? (r-unfolded-type? app column-selector type-id)]
     (fn [app column-selector type-id seen]
       (let [type-eid (:db/id @type-entity)]
         (if-not (seen type-eid)
           [re-com/v-box
            :style {:padding-left "10px"}
            :children [[type-folder app column-selector @type-entity]
                       (when-not @unfolded?
                         [re-com/v-box
                          ;; First, render the same thing for the subtypes, so their attributes can show up if unfolded
                          :children [[re-com/v-box
                                      :children (for [subtype-id (map :db/id (:e.type/_isa @type-entity))]
                                                  ^{:key subtype-id}
                                                  [attribute-column-selector-rows app column-selector subtype-id (conj seen type-eid)])]
                                     ;; This is all of the types directly assigned attribute selection rows
                                     [re-com/v-box
                                      :children (for [attr-entity (:e.type/attributes @type-entity)]
                                                  ^{:key (:db/id attr-entity)}
                                                  [attribute-column-selector-row app column-selector attr-entity (conj seen type-eid)])]]])]]
           [re-com/label :label "This type has already been specified"]))))))

(defn collapsed?
  [])

(representation/register-representation
  ::column-selector
  (fn [app [_ context-data] column-selector-id]
    (let [collapse? (r/atom true)]
      (fn [app [_ context-data] column-selector-id]
        (let [base-type (::base-type context-data)]
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
                         ;; TODO Switch to posh/entid when that is implemented
                         :child (let [base-type-id (:db/id @(posh/pull (:conn app) '[:db/id] base-type))]
                                  [attribute-column-selector-rows app column-selector-id base-type-id])])]])))))

; Build a magical selector for attributes
(defn attribute-column-selector
  "The top level attribute column selector component; based on type eid or lookup ref (like [:db/ident :e.type/Comment])."
  [app column-selector base-type]
  [representation/represent app [::column-selector {::base-type base-type}] column-selector])


(representation/register-representation
  ::row-value-view
  (fn [app [_ context-data] value]
    [:td {:style {:padding "4px 8px"}}
     ;; If here we know in context what the path is to the data, we should pass that along as well
     (if-let [path (::path context-data)]
       ;; Then we should have enough info to use :dat.view/value-view
       ;; XXX Note; this hasn't been tested yet and is probably broken, so dont pass path and use generic for now
       ;; Also make sure to rewrite with attr-ident in context
       (let [attr-ident (last path)
             context-data' (assoc context-data ::path path
                                               :attribute/ident attr-ident)]
         [dat.view/represent app [:dat.view/value-view context-data'] value])
       ;; Otherwise, just stringify; This could also maybe be a separate mm dispatch
       (str value))]))


;; Context data should have
(representation/register-representation
  ::row-view
  (fn [app [_ context-data] row]
    [:tr
     ;; If here we know in context what the path is to the data, we should pass that along as well
     (for [[i value] (map-indexed vector row)]
       (let [path (nth (::paths context-data) i nil)]
         ^{:key i}
         [dat.view/represent app
                             [::row-value-view (assoc context-data
                                                 ;; Shoud be associng in the attr-ident as well
                                                 ::path path
                                                 :db.attr/ident (last path)
                                                 ::row-index i)]
                             value]))]))


(defn entity-row-view
  ([app context-data paths row]
   [dat.view/represent app [::row-view (merge context-data {::paths paths})] row])
  ([app paths row]
   [entity-row-view app {} paths row]))


(defn path-name
  [path]
  (if-let [path-names (seq (map name path))]
    (clojure.string/join "/" path-names)
    "base-eid"))

(defn ordered-paths
  [{:as query-context :keys [query sym-mapping]}]
  (mapv sym-mapping (:find query)))


;; Again, this is gonna have to totally change once we refactor to accept (and probably prefer) pull
(representation/register-representation
  ::header-view
  (fn [app [_ context-data] _]
    (let [{:keys [query sym-mapping]} context-data
          find-syms (:find query)]
      [:tr
       (for [sym find-syms]
         (let [path (sym-mapping sym)]
           ^{:key (hash path)}
           ;; TODO This should really be based on :dat.view/attr-label
           [:th
            {:style {:padding "8px"}}
            (path-name path)]))])))


(defn header-view
  [app context-data]
  [dat.view/represent app [::header-view context-data] nil])


;; Writing out results

;(defn evaluate-query
  ;[app query-context eids]
  ;(posh/q (:query query-context) (:conn app) eids))

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

;; XXX Would really like to use the js papa lib for this but can't figure out how to do cljsjs
;(println (format-csv [["1,000" "2" "3" "frank,this"] [4 "5,\"000" "6"]]))

(defn download-csv
  [paths rows]
  (let [csv-content "data:text/csv;charset=utf-8,"
        rows (vec (concat [(mapv path-name paths)]
                          rows))
        csv-content (str csv-content (format-csv rows))
        encode-uri (js/encodeURI csv-content)]
    (.open js/window encode-uri)))


;; Here we should be able to extend the table view via the context interpretation
;; But for right now just assuming data is eids
(representation/register-representation
  :dat.view/table-view
  (fn [app [_ context-data] [base-type eids]]
    (let [column-selector (::column-selector context-data)
          ;base-type (::base-type context-data)
          conn (:conn app)
          conn-reaction (dat.view/as-reaction conn)
          query-context (type-query-reaction conn-reaction base-type)]
          ;query-results (evaluate-query conn query-context eids)]
      (fn [app [_ context-data] [base-type eids]]
        (let [ordered-paths (ordered-paths @query-context)
              ;; Question: What if conn changes? Compute in inner fn?
              ;; TODO Should translate this to posh/q
              rows @(dat.view/safe-q (:query @query-context) conn eids)]
          [re-com/v-box
           :gap "15px"
           :children [[re-com/title :level :level2 :label "Table view"]
                      [re-com/h-box
                       :gap "20px"
                       :children [[re-com/md-icon-button
                                   :md-icon-name "zmdi-download"
                                   :tooltip "Download table as CSV"
                                   :on-click (partial download-csv ordered-paths rows)]
                                  [attribute-column-selector app column-selector base-type]]]
                      [:table
                       [:tbody
                        [header-view app @query-context]
                        (for [row rows]
                          ^{:key (hash row)}
                          [dat.view/represent app [::row-view context-data] row])]]]])))))
                          ;[entity-row-view ordered-paths row])]]]]))))


;; This is more or less deprecated and going to be rewritten, so don't build on it for now.
;; Backwards compatibility
(defn table-view
  ;; Should generate a column-selector from
  [app column-selector base-type eids]
  (dat.view/represent app
                      [:dat.view/table-view {::mode ::eids
                                             ::base-type base-type
                                             ::column-selector column-selector}]
                      eids))

