(ns dat.view
  "# Datview"
  (:require-macros [reagent.ratom :refer [reaction]]
                   [cljs.core.async.macros :as async-macros :refer [go go-loop]])
  (:require [dat.reactor :as reactor]
            [dat.reactor.dispatcher :as dispatcher]
            [dat.view.representation :as representation]
            [dat.view.router :as router]
            [dat.view.utils :as utils]
            [dat.view.context :as context]
            [dat.view.query :as query]
            [dat.view.routes :as routes]
            [dat.view.settings :as settings]
            [dat.spec.protocols :as protocols]
            ;; Things outside datsys, but intimately tied to datsys
            [datascript.core :as d]
            [posh.reagent :as posh]
            [reagent.core :as r]
            [reagent.ratom :as ratom]
            [re-com.core :as re-com]
            ;; Other stuff
            [datafrisk.core :as frisk]
            [taoensso.timbre :as log :include-macros true]
            [com.stuartsierra.component :as component]
            [clojure.walk :as walk]
            [goog.date.Date]
            [cljs-time.core :as cljs-time]
            [cljs.core.async :as async]
            [cljs.spec :as s]
            [cljs-time.format]
            [cljs-time.coerce]
            [cljs.pprint :as pp]
            [cljs.core.match :as match :refer-macros [match]]
            [markdown.core :as md]
            [dat.view.styles :as styles]))





;; ## Represent

;; This is really the cornerstone of all of dat.view
;; This multimethod represents the abstract ability to render/represent something based on abstract context

(def represent
  "Maps args `[app context data]` to a representation (hiccup, most likely) as dispatched by (first context). Representations can
  be added via register-representation.

  Note: State is currently in a var in dat.view.representation; There will maybe eventually be a default such, but it
  would be good to make it possible to not do that."
  representation/represent)

(def register-representation
  "Registers a representation function (maping of args args `[app context data]` to a view representation) for a given
  context-id (the first value of `context := [context-id context-data]`). as dispatched by (first context). Representations can
  be added via register-representation.

  Note: State is currently in a var in dat.view.representation; There will maybe eventually be a default such, but it
  would be good to manage the state yourself."
  representation/register-representation)



;; ## Events

;; Speccing some things out about events.
;; Much of this should all get moved over to the datspec namespace probably.
;; Only datview specific things should stay here.


(s/def ::event-id (s/and keyword? namespace))

(s/def ::event (s/and vector? (s/cat :event-id  ::event-id
                                     :event-data (constantly true))))

(s/def ::conn d/conn?)

(s/def ::dispatcher #(satisfies? protocols/PDispatcher %))

;; TODO:
(s/def ::base-context map?)



;;abstraction over a sente connection
(s/def ::remote (s/and #(satisfies? protocols/PRemoteSendEvent %)
                       #(satisfies? protocols/PRemoteEventChan %)))


(s/def ::app (s/keys :req-un [::conn ::dispatcher ::base-context]
                     :opt-un [::remote]))



;; Some wrappers for convenience

(defn dispatch!
  ([app event]
   (dispatcher/dispatch! (:dispatcher app) event))
  ([app event level]
   (dispatcher/dispatch! (:dispatcher app) event level)))

(s/def ::dispatch-args (s/cat :app ::app :event ::event :level (s/? keyword?)))

(s/fdef dispatch!
        :args ::dispatch-args
        :ret (constantly true))

(defn dispatch-error!
  [app event]
  (dispatcher/dispatch-error! (:dispatcher app) event))

(s/fdef dispatch-error!
        :args (s/cat :app ::app :event ::event)
        :ret (constantly true))


;; TODO Should rename send-remote-tx!
(defn send-tx!
  "Helper function for dispatching tx messages to server."
  [app tx-forms]
  ;; TODO This should be smarter, and look to see whether dat.sys is loaded, and dispatch occordingly
  (dispatch! app [:dat.sync.client/send-remote-tx tx-forms]))


(defn send-remote-event!
  [app remote-event]
  (dispatch! app [:dat.remote/send-event! remote-event]))




;; Importing styles, etc

(def box-styles styles/box-styles)
(def h-box-styles styles/h-box-styles)
(def v-box-styles styles/v-box-styles)
(def bordered-box-style styles/bordered-box-style)

;; ## Metadata view specification structure defaults

(defn box
  "Prefers children over child"
  [{:as args :keys [style children child]}]
  [:div {:style (merge box-styles style)}
   ;; Not sure yet if this will work as expected
   (or (seq children) child)])

;; For debugging

(defn debug-str
  ([message data]
   (str message (debug-str data)))
  ([data]
   (with-out-str (pp/pprint data))))

(defn debug
  ([message data]
   [:div.debug
    [:p message]
    [:pre {:style {:max-height "300px" :overflow-y "auto"}} (debug-str data)]])
  ([data]
   (debug "" data)))



;; Default view when a representation can't be found


(representation/register-representation
  :default
  (fn [_ context data]
    [:div
     [:h4 "No representation for:"]
     [debug "context:" context]
     [debug "data:" data]]))



(def base-pull
  [* {:e/type [*]}])



;; ## Reactions

(def deref-or-value utils/deref-or-value)
(def deep-merge utils/deep-merge)
(def as-reaction utils/as-reaction)
(def safe-pull utils/safe-pull)
(def safe-q utils/safe-q)
(def pull-many utils/pull-many)
(def pull-attr utils/pull-attr)
(def pull-path utils/pull-path)


;; Context

(def default-base-context context/default-base-context)
(def base-context context/base-context)
(def update-base-context! context/update-base-context!)
(def set-base-context! context/set-base-context!)
(def attribute-signature-reaction context/attribute-signature-reaction)
(def attribute-schema-reaction context/attribute-schema-reaction)
(def component-context context/component-context)



;; ## DataScript schema

;; Some basic schema that needs to be transacted into the database in order for these functions to work

(def base-schema
  {:dat.view.base-context/value {}})

(def default-settings
  [{:db/ident ::base-context
    :dat.view.base-context/value {}}])

;; Have to think about how styles should be separated from container structure, etc, and how things like
;; little control bars can be modularly extended, etc.
;; How can this be modularized enough to be truly generally useful?

;; These should be moved into styles ns or something



;; ## Client Helper components

;; Need to think about the abstract shape of a control like a button

(defn loading-notification
  [message]
  [re-com/v-box
   :style {:align-items "center"
           :justify-content "center"}
   :gap "15px"
   :children
   [[re-com/title :label message]
    [re-com/throbber :size :large]]])


(defn collapse-button
  "A collapse button for hiding information; arg collapse? should be a bool or an ratom thereof.
  If no click handler is specified, toggles the atom."
  ([collapse? on-click-fn]
   (let [[icon-name tooltip] (if (utils/deref-or-value collapse?) ;; not positive this will work the way I expect
                               ["zmdi-caret-right" "Expand collection"]
                               ["zmdi-caret-down" "Hide collection"])]
     [re-com/md-icon-button :md-icon-name icon-name
                            :tooltip tooltip
                            :on-click on-click-fn]))
  ([collapse?]
   (collapse-button collapse? (fn [] (swap! collapse? not)))))



;; ## Builder pieces

;; These are builder pieces part of the public api;
;; These should be accessible for wrapping, and should be overridable/extensible via correspondingly named keys of the context map at various entry points


(representation/register-representation
  ::pull-summary-string
  (fn [_ _ pull-data]
    ;[:span
     (match [pull-data]
       [{:e/name name}] name
       [{:attribute/label label}] label
       [{:db/ident ident}] (name ident)
       [{:e/type {:db/ident type-ident}}] (str (name type-ident) " instance")
       ;; A terrible assumption really, but fine enough for now
       :else (pr-str pull-data))))

(defn pull-summary-string
  ([app pull-data]
   (pull-summary-string app {} pull-data))
  ([app context pull-data]
   [represent app [::pull-summary-string context] pull-data]))


(representation/register-representation
  ::pull-summary-view
  (fn [app [_ context] pull-data]
    [:div {:style {:font-weight "bold" :padding "5px" :align-self "end"}}
     [represent app [::pull-summary-string (:dat.view.context/locals context) pull-data]]]))

(defn pull-summary-view
  [app context pull-data]
  [represent app [::pull-summary-view (:dat.view.context/locals context)] pull-data])


(representation/register-representation
  ::collapse-summary
  (fn [app [_ context] values]
    ;; XXX Need to stylyze and take out of re-com styling
    (let [local-context (:dat.view.context/locals context)]
      (if (map? values)
        [represent app [::pull-summary-view local-context] values]
        [:div {:style (merge h-box-styles
                             {:padding "10px"})}
                              ;:align :end})}
                              ;:gap "20px"
         ;[debug "collapse-vals: " values]
         (for [value (distinct values)]
           ^{:key (hash value)}
           [represent app [::pull-summary-view local-context] value])]))))

(defn collapse-summary
  [app context values]
  [represent app [::collapse-summary (:dat.view.context/locals context)] values])



;; ## Attribute view

;; View all of the values for some entity, attribute pair
;; Values must be passed in explicitly, or in an atom

(defn lablify-attr-ident
  [attr-ident]
  (let [[x & xs] (clojure.string/split (name attr-ident) #"-")]
    (clojure.string/join " " (concat [(clojure.string/capitalize x)] xs))))


(representation/register-representation
  ::label-view
  (fn [app _ attr-ident]
    [:div
     (when attr-ident
       [re-com/label
        :style {:font-size "14px"
                :font-weight "bold"}
        :label
        (or (:attribute/label @(safe-pull (:conn app) [:db/id :db/ident :attribute/label] [:db/ident attr-ident]))
            (lablify-attr-ident attr-ident))])]))

(defn label-view
  "For a given attr-ident, render a label for that attribute."
  ([app context attr-ident]
   (let [local-context (:dat.view.context/locals context)]
     [represent app [::label-view local-context] attr-ident]))
  ([app attr-ident]
   (label-view app {} attr-ident)))


(defn get-nested-pull-expr
  [pull-expr attr-ident]
  (or
    (some (fn [attr-entry]
             (cond
               ;; Not sure if these :component assignments are the right ticket
               (and (keyword? attr-entry) (= attr-entry attr-ident))
               ^{:component summary-view} '[*]
               (and (map? attr-entry) (get attr-entry attr-ident))
               (get attr-entry attr-ident)
               :else false))
          pull-expr)
    ^{:component summary-view} '[*]))

;; Summary needs to be handled somewhat more cleverly... Set up as a special function that returns the corresponding pull-expr component?


;; Still need to hook up with customized context

(representation/register-representation
  ::copy-entity-control
  (fn [app _ pull-data]
    (let [pull-data (utils/deref-or-value pull-data)]
      ;; TODO Need to figure out the right way to configure the re-com components
      [re-com/md-icon-button :md-icon-name "zmdi-copy"
       :size :smaller
       :style {:margin-right "10px"}
       :tooltip "Copy entity" ;; XXX Should make tooltip fn-able
       :on-click (fn [] (js/alert "Coming soon to a database application near you"))])))

(representation/register-representation
  ::edit-entity-control
  (fn [app _ pull-data]
    (let [pull-data (utils/deref-or-value pull-data)]
      [re-com/md-icon-button :md-icon-name "zmdi-edit"
       :style {:margin-right "10px"}
       :size :smaller
       :tooltip "Edit entity"
       ;; This assumes the pull has :dat.sync.remote.db/id... automate?
       :on-click (fn [] (router/set-route! app {:handler :edit-entity :route-params {:db/id (:dat.sync.remote.db/id pull-data)}}))])))


;; TODO Need a way to figure out which controls are needed for a given component
(defn component-controls
  [context]
  ;; For now..
  (::controls context))

(representation/register-representation
  ::control-set
  (fn [app [_ context] data]
    (let [local-context (:dat.view.context/locals context)]
      ;; XXX This was ::pull-view-controls, now ::control-set
      [:div (:dom/attrs context)
       (for [control-id (distinct (component-controls context))]
         ^{:key (hash control-id)}
         [represent app [control-id local-context] data])])))



;; TODO Set up defaults ::copy-entity-control ::edit-entity-control
;(defn default-pull-view-controls
;  [app context pull-data]
;  (let [context [::controls (assoc context ::controls [::copy-entity-control ::edit-entity-control])]]
;    (represent app context pull-data)))


;; Hmm... decided I don't like the [attr-ident value] thing; attr-ident should really be part of the context.
;; If not present, should just default.
;; Maybe slightly less than ideal for refs, but still valueable I think.
;; Attr ident is really just context, which we may or may not need.
;; Update: Still waffling on this... Went the other way in forms; maybe better to be consistent...
(representation/register-representation
  ::value-view
  ;; QUESTION Should attr-ident be part of the context-data?
  (fn [app [_ context] value]
    (let [attr-ident (:db.attr/ident context)
          attr-sig @(attribute-signature-reaction app attr-ident)]
      [:div (:dom/attrs context)
       ;[debug "context dom/attrs:" (:dom/attrs context)]
       (match [attr-sig]
         ;; For now, all refs render the same; May treat component vs non-comp separately later
         [{:db/valueType :db.type/ref}]
              ;; This is something that will need to be generalized
         (let [nested-context (update (:dat.view.context/locals context) ::pull-expr get-nested-pull-expr attr-ident)]
           ;; QUESTION: Where should the nsted pull-expr go?
           [represent app [::pull-data-view nested-context] value])
         ;; Miscellaneous value
         :else
         (str value))])))


;; TODO Need to figure out the signature here
;(defn value-view
;  [app pull-expr attr-ident value]



;; Should we have a macro for building these components and dealing with all the state in the context? Did the merge for you?
;(defn build-view-component)

(representation/register-representation
  ::attr-values-view
  (fn [app [_ context] values]
    (let [;; Should put all of the collapsed values in something we can serialize, so we always know what's collapsed
          collapse-attribute? (r/atom (::collapsed? context))
          local-context (:dat.view.context/locals context)]
      (fn [app [_ context] values]
        (let [collapsable? (::collapsable? context)]
          [:div (:dom/attrs context)
           (when collapsable?
             [collapse-button collapse-attribute?])
           (when @collapse-attribute?
             [collapse-summary app local-context values])
           ;(defn pull-summary-view [app pull-expr pull-data]
           (when (or (not collapsable?) (and collapsable? (not @collapse-attribute?)))
             (for [value (utils/deref-or-value values)]
               ^{:key (hash value)}
               [represent app [::value-view local-context] value]))])))))


;(defn attr-values-view
;  [app context attr-ident values])


;; Need to have controls etc here
(representation/register-representation
  ::attr-view
  (fn [app [_ context] values]
    (let [attr-ident (:db.attr/ident context)
          attr-signature @(attribute-signature-reaction app attr-ident)
          local-context (:dat.view.context/locals context)
          child-context (merge local-context (get-in local-context [::ref-attrs attr-ident]))]
      [:div (:dom/attrs context)
       [represent app [::label-view (assoc child-context ::attr-signature attr-signature)] attr-ident]
       (match [attr-signature]
         [{:db/cardinality :db.cardinality/many}]
         [represent app [::attr-values-view child-context] values]
         :else
         [represent app [::value-view child-context] values])])))


;(defn attr-view
;  [app pull-expr attr-ident values])


;; All rendering modes should be controllable via registered toggles or fn assignments
;; registration modules for plugins
;; * middleware?

(defn pull-attributes
  ([pull-expr pull-data]
   (-> pull-expr
       deref-or-value
       (->> (map (fn [attr-spec]
                   (cond
                     (keyword? attr-spec) attr-spec
                     (map? attr-spec) (keys attr-spec)
                     (symbol? attr-spec)
                     (case attr-spec
                       '* (filter
                            (set (pull-attributes (remove #{'*} pull-expr) []))
                            (keys (utils/deref-or-value pull-data))))))))
       flatten
       (concat (keys pull-data))
       distinct))
  ([pull-expr]
   (pull-attributes pull-expr [])))


(representation/register-representation
  ::pull-data-view
  (fn [app [_ context] pull-data]
    ;; Annoying to have to do this
    (let [;; TODO Ignoring the component context
          ;; TODO Insert collapse here
          ;; here we go on collapse
          collapse-attribute? (r/atom (::collapsed? context))]
      (fn [app [_ context] pull-data]
        (let [local-context (:dat.view.context/locals context)
              collapsable? (::collapsable? context)
              pull-expr (::pull-expr context)
              pull-data (utils/deref-or-value pull-data)]
          [:div {:style h-box-styles}
           (when collapsable?
             [collapse-button collapse-attribute?])
           (when @collapse-attribute?
             [collapse-summary app context pull-data])
           ;(defn pull-summary-view [app pull-expr pull-data]
           (when (or (not collapsable?) (and collapsable? (not @collapse-attribute?)))
             ;(for [value (utils/deref-or-value pull-data)]
             ;  ^{:key (hash value)}
             ;  [represent app [::value-view context] value]
             [:div (:dom/attrs context)
              ;[debug "Pull data view context: " context]
              [:div
                [represent app [::control-set local-context] pull-data]
                [:div {:style (merge h-box-styles)}
                 [represent app [::pull-summary-view local-context] pull-data]]]
              ;; XXX TODO Questions:
              ;; Need a react-id function that lets us repeat attrs when needed
              (for [attr-ident (pull-attributes pull-expr pull-data)]
                ^{:key (hash attr-ident)}
                [represent app [::attr-view (assoc local-context :db.attr/ident attr-ident)] (get pull-data attr-ident)])])])))))


;; See definition below
(declare meta-context)

;; Note that here we extract the meta-context from the pull-expr

(representation/register-representation
  ::pull-view
  (fn [app [_ context] [pull-expr eid]]
    (let [pull-data (safe-pull (:conn app) pull-expr eid)
          child-context (-> (:dat.view.context/locals context)
                            ;; !!! Extract and merge the metadata context from the pull expression
                            (merge (meta-context pull-expr))
                            (assoc ::pull-expr pull-expr))]
      ;; TODO We are also associng in the pull expr above somewhere; Should make these play nice together and decide on precedence
      [:div
       [represent app [::pull-data-view child-context] pull-data]])))


(defn pull-data-view
  "Given a DS connection, a app pull-expression and data from that pull expression (possibly as a reaction),
  render the UI subject to the pull-expr metadata."
  ;; Should be able to bind the data to the type dictated by pull expr
  ([app context pull-data]
   [represent app [::pull-data-view context] pull-data])
  ([app pull-data]
   (pull-data-view app {} pull-data)))

(defn pull-view
  ([app context pull-expr eid]
   [represent app [::pull-view context] [pull-expr eid]])
  ([app pull-expr eid]
   (pull-view app (meta-context pull-expr) pull-expr eid)))

;; General purpose sortable collections in datomic/ds?
;; Should use :attribute/sort-by; default :db/id?


(defn attr-sort-by
  [app attr-ident]
  (reaction (or (:db/ident (:attribute/sort-by @(safe-pull (:conn app) '[:db/ident] [:db/ident attr-ident])))
                ;; Should add smarter option for :e/order as a generic? Or is this just bad semantics?
                :db/id)))

(defn value-type
  [app attr-ident]
  (reaction (:db/valueType @(safe-pull (:conn app) '[*] [:db/ident attr-ident]))))

(defn reference?
  [app attr-ident values]
  (reaction (= (value-type app attr-ident) :db.type/ref)))

;; Can add matches to this to get different attr-idents to match differently; Sould do multimethod?
;; Cardinality many ref attributes should have an :attribute.ref/order-by attribute, and maybe a desc option
;; as well
(defn sorted-values
  [app attr-ident values]
  (reaction (if @(reference? app attr-ident values)
              (sort-by @(attr-sort-by app attr-ident) values)
              (sort values))))




;; ## Forms!!


;; I've decide to move everything over here, since it will now be assumed that if you want datview, you want it's form functionality
;; Not sure if this makes sense or not yet, but it's my running design.

;; Holy shit... there's gonna be a lot of work to do here...
;; Need to rewrite everything in terms of represent


(declare pull-form)
(declare pull-data-form)

(defn cast-value-type
  [value-type-ident str-value]
  (case value-type-ident
    (:db.type/double :db.type/float) (js/parseFloat str-value)
    (:db.type/long :db.type/integer) (js/parseInt str-value)
    str-value))


;; TODO Rewrite in terms of event registration
(defn make-change-handler
  "Takes an app, an eid attr-ident and an old value, and builds a change handler for that value"
  [app eid attr-ident old-value]
  ;; This whole business with the atom here is sloppy as hell... Will have to clean up with smarter delta
  ;; tracking in database... But for now...
  (let [current-value (r/atom old-value)
        value-type-ident (d/q '[:find ?value-type-ident .
                                :in $ % ?attr-ident
                                :where (attr-ident-value-type-ident ?attr-ident ?value-type-ident)]
                              @(:conn app)
                              query/rules
                              attr-ident)]
    (fn [new-value]
      (let [old-value @current-value
            new-value (cast-value-type value-type-ident new-value)]
        (when (not= old-value new-value)
          ;; This isn't as atomic as I'd like XXX
          (reset! current-value new-value)
          (send-tx!
            app
            (concat
              (when old-value [[:db/retract eid attr-ident old-value]])
              ;; Probably need to cast, since this is in general a string so far
              [[:db/add eid attr-ident new-value]])))))))


(defn apply-reference-change!
  ([app eid attr-ident new-value]
   (apply-reference-change! app eid attr-ident nil new-value))
  ([app eid attr-ident old-value new-value]
   (let [old-value (match [old-value]
                          [{:db/id id}] id
                          [id] id)]
     (send-tx! app
               (concat [[:db/add eid attr-ident new-value]]
                       (when old-value
                         [[:db/retract eid attr-ident old-value]]))))))

(defn inspect
  [stuff]
  (doseq [x stuff]
    (log/debug "label: " (:label x)))
  stuff)

(def ref-attr-options
  (memoize
    (fn
      ([app attr-ident]
       (ref-attr-options app attr-ident :db/id))
      ([app attr-ident sort-key]
       (->>
         (safe-q '[:find [(pull ?e [:db/id :db/ident * {:e/type [*]}]) ...]
                   :in $ % ?attr
                   :where [?attr :attribute.ref/types ?type]
                          (type-instance ?type ?e)]
                 (:conn app)
                 query/rules
                 [:db/ident attr-ident]))))))
         ;deref
         ;(mapv
         ;  (fn [pull-data]
         ;    (assoc pull-data
         ;      ;; XXX pull-summary-string does not actually return a string... need to fix this
         ;      :label (or (pull-summary-string app pull-data) "NA")
         ;      :id (:db/id pull-data))))
         ;; XXX There needs to be more control over the ordering here, via context or something; Explicitly passing options is a punt
         ;inspect
         ;(sort-by :db/id)
         ;vec
         ;reaction)))))

;; TODO Need constext here for a better sort-by specification; switch to representation
(defn select-entity-input
  ([app eid attr-ident value]
    ;; XXX value arg should be safe as a reaction here
   (let [options (ref-attr-options app attr-ident :label)]
         ;; Should extract this as something memoized
     ;; Remove this div; just for debug XXX
     [select-entity-input app eid attr-ident value options]))
  ([app eid attr-ident value options]
   (let [value (utils/deref-or-value value)
         value (or (:db/id value)
                   (and (vector? value) @(pull-attr (:conn app) value :db/id))
                   value)]
     [re-com/single-dropdown
      :style {:min-width "150px"}
      :choices options
      ;; TODO Not sure if this will break things or not; have to test
      ;:model (:db/id value)
      :id-fn :db/id
      ;; For now hard coding this... For some reason using the summary function here is messing everything up
      :label-fn :e/name
      :model value
      :on-change (partial apply-reference-change! app eid attr-ident value)])))


;; Simple md (markdown) component; Not sure if we really need to include this in dat.view or not...
(defn md
  [md-string]
  [re-com/v-box
   :children
   [[:div {:dangerouslySetInnerHTML {:__html (md/md->html md-string)}}]]])


;; ### Datetimes...

;; TODO Need to get proper date+time handlers that handle both the date and the time

;(defn update-date
;  [old-instant new-date]
;  ;;; For now...
;  (let [old-instant (cljs-time.coerce/from-date old-instant)
;        day-time (cljs-time/minus old-instant (cljs-time/at-midnight old-instant))
;        new-time (cljs-time/plus new-date day-time)]
;    new-time))

(defn update-date
  [old-instant new-date]
  ;; For now...
  new-date)

(defn datetime-date-change-handler
  [app eid attr-ident current-value new-date-value]
  (let [old-value @current-value
        new-value (update-date old-value new-date-value)]
    (reset! current-value new-value)
    (send-tx! app
              (concat (when old-value
                        [[:db/retract eid attr-ident (cljs-time.coerce/to-date old-value)]])
                      [[:db/add eid attr-ident (cljs-time.coerce/to-date new-value)]]))))

;; XXX Finish
(defn datetime-time-change-handler
  [app eid attr-ident current-value new-time-value]
  ())

(defn timeint-from-datetime
  [datetime])


(defn datetime-selector
  [app eid attr-ident value]
  (let [current-value (atom value)]
    (fn [app eid attr-ident value]
      [:datetime-selector
       [re-com/datepicker-dropdown :model (cljs-time.coerce/from-date (or @current-value (cljs-time/now)))
        :on-change (partial datetime-date-change-handler app eid attr-ident current-value)]])))
;[re-com/input-time :model (timeint-from-datetime @current-value)
;:on-change (partial datetime-time-change-handler app eid attr-ident current-value)]


(defn boolean-selector
  [app eid attr-ident value]
  (let [current-value (atom value)]
    (fn []
      [re-com/checkbox :model @current-value
       :on-change (fn [new-value]
                    (let [old-value @current-value]
                      (reset! current-value new-value)
                      (send-tx! app
                                (concat
                                  (when-not (nil? old-value)
                                    [[:db/retract eid attr-ident old-value]])
                                  [[:db/add eid attr-ident new-value]]))))])))


;; XXX Having to do a bunch of work it seems to make sure that the e.type/attributes properties are set up for views to render properly;
;; We're not getting time entries showing up on ui;
;; Not sure if not making the circuit or if something weird is going on.

;; XXX Also, it seems like right now we need the :db/id in the pull expressions; Need to find a way of requesting for other data when needed

;; XXX Should have option for collapse that would let you collapse all instances of some attribute, versus just one particular entity/attribute combo

;; Should have this effectively mutlitmethod dispatch using the dat.view customization functionality
(defn input-for
  ([app context pull-expr eid attr-ident value]
   (let [child-context (assoc (:dat.view.context/locals context)
                         ::pull-expr pull-expr)]
     [represent app [::input-for child-context] [eid attr-ident value]])))


(representation/register-representation
  ::input-for
  (fn [app [_ context] [eid attr-ident value]]
    ;; TODO Need to rewrite in terms of representations
    (let [attr @(attribute-signature-reaction app attr-ident)
          pull-expr (::pull-expr context)
          local-context (:dat.view.context/locals context)]
      ;[:div
      ; [represent app [::control-set]]]
      (match [attr]
        ;; The first two forms here have to be compbined and the decision about whether to do a dropdown
        ;; left as a matter of the context (at least for customization); For now leaving though... XXX
        ;; We have an isComponent ref; do nested form
        ;; Should this clause just be polymorphic on whether value is a map or not?
        [{:db/valueType :db.type/ref :db/isComponent true}]
        ;; Need to assoc in the root node context here
        (let [sub-expr (some #(get % attr-ident) pull-expr) ;; XXX This may not handle a ref not in {}
              ;; Need to handle situation of a recur point ('...) as a specification; Should be the context pull root, or the passed in expr, if needed
              sub-expr (if (= sub-expr '...) (or (:dat.view/root-pull-expr context) pull-expr) sub-expr)
              local-context (if (:dat.view/root-pull-expr context)
                              context
                              (assoc context :dat.view/root-pull-expr pull-expr))]
          ;(when-not (= (:db/cardinality attr) :db.cardinality/many)
          ;;(nil? value))
          [pull-form app local-context sub-expr value])
        ;; This is where we can insert something that catches certain things and handles them separately, depending on context
        ;[{:db/valueType :db.type/ref} {:dat.view.level/attr {?}}]
        ;[pull-form app context-data (get pull-expr value)]
        ;; TODO Need to redo all the below as representations
        ;; Non component entity; Do dropdown select...
        [{:db/valueType :db.type/ref}]
        [select-entity-input app eid attr-ident value]
        ;; Need separate handling of datetimes
        [{:db/valueType :db.type/instant}]
        [datetime-selector app eid attr-ident value]
        ;; Booleans should be check boxes
        [{:db/valueType :db.type/boolean}]
        [boolean-selector app eid attr-ident value]
        ;; For numeric inputs, want to style a little differently
        [{:db/valueType (:or :db.type/float :db.type/double :db.type/integer :db.type/long)}]
        [re-com/input-text
         :model (str value)
         :width "130px"
         :on-change (make-change-handler app eid attr-ident value)]
        ;; Misc; Simple input, but maybe do a dynamic type dispatch as well for customization...
        :else
        [re-com/input-text
         :model (str value) ;; just to make sure...
         :width (if (= attr-ident :db/doc) "350px" "200px")
         :on-change (make-change-handler app eid attr-ident value)]))))


(defn create-type-reference
  [app eid attr-ident type-ident]
  (send-tx!
    app
    ;; Right now this also only works for isComponent :db.cardinality/many attributes. Should
    ;; generalize for :db/isComponent false so you could add a non-ref attribute on the fly XXX
    ;; This also may not work if you try to transact it locally, since type-ident doesn't resolve to the entity in DS (idents aren't really supported) XXX
    ;; Could maybe work with a ref [:db/ident type-ident], but I don't know if these are supported in tx
    [{:db/id -1 :e/type type-ident}
     [:db/add eid attr-ident -1]]))


;; TODO Need to rewrite in terms of representations
(defn attr-type-selector
  [type-idents selected-type ok-fn cancel-fn]
  ;; Right now only supports one; need to make a popover or something that asks you what type you want to
  ;; create if there are many possible... XXX
  [re-com/v-box
   ;:style {:width "500px" :height "300px"}
   :children
   [[re-com/title :label "Please select an entity type"]
    [re-com/single-dropdown
     :choices (mapv (fn [x] {:id x :label (pr-str x)}) type-idents)
     :model selected-type
     :style {:width "300px"}
     :on-change (fn [x] (reset! selected-type x))]
    [re-com/h-box
     :children
     [[re-com/md-icon-button :md-icon-name "zmdi-check"
       :size :larger
       :style {:margin "10px"}
       :tooltip "add selected entity"
       :on-click ok-fn]
      [re-com/md-icon-button :md-icon-name "zmdi-close-circle"
       :size :larger
       :style {:margin "10px"}
       :tooltip "Cancel"
       :on-click cancel-fn]]]]])


;; All this skeleton stuff is a bit anoying; these things are what the user should be specifying, not the
;; other way around
;; Should strip down and simplify field-for-skeleton; Doesn't need to be this complex XXX
;; TODO Need to rewrite in terms of representations, or write this one in terms of a layout, if that becomes a separate notion
(defn field-for-skeleton
  [app attr-ident controls inputs]
  [re-com/v-box
   :style {:flex-flow "column wrap"}
   :padding "10px"
   :children
   [;; First the label view, and any label controls that might be needed
    [re-com/h-box
     :style {:flex-flow "row wrap"}
     :children
     [[label-view app attr-ident]
      [re-com/h-box :children controls]]]
    ;; Put our inputs in a v-box
    [re-com/v-box
     :style {:flex-flow "column wrap"}
     :children inputs]]])

;; TODO Need to rewrite in terms of control represetnations (and make more abstract and ref attr-type based)
(defn add-reference-button
  "Simple add reference button"
  ([tooltip on-click-fn]
   [re-com/md-icon-button
    :md-icon-name "zmdi-plus"
    :size :smaller
    :on-click on-click-fn
    :tooltip tooltip])
  ([on-click-fn]
   (add-reference-button "Add entity" on-click-fn)))

;; Similarly, should have another function for doing the main simple operation here XXX
(defn add-reference-for-type-button
  "Simply add a reference for a given type (TODO...)"
  [tooltip type-ident])

;; We should rewrite the main use case below to use this function istead of the one above; reduce complexity
;; TODO Need to rewrite in terms of representation
(defn add-reference-button-modal
  "An add reference button that pops up a modal form with a submit button.
  modal-popup arg should be a component that takes param:
  * form-activated?: an atom with a bool indicating whether the form should be shown.
  This component should make sure to toggle form-activated? when it's done creating
  the component, or if there is a cancelation."
  ([tooltip modal-popup]
   (let [form-activated? (r/atom false)]
     (fn [tooltip modal-popup]
       [re-com/v-box
        :children
        [[add-reference-button tooltip (fn [] (reset! form-activated? true))]
         (when @form-activated?
           [re-com/modal-panel :child [modal-popup form-activated?]])]])))
  ([modal-popup]
   (add-reference-button "Add entity" modal-popup)))


;; Again; need to think about the right way to pass through the attribute data here
;
(representation/register-representation
  ::fields-for
  ;; So first we get attr-signature and config
  ;; TODO Should make this also ok with not passing in the value(s) so that it can pull for you...
  (fn [app [_ context] [eid attr-ident value]]
    (let [attr-sig (attribute-signature-reaction app attr-ident)
          ;; Should move all this local state in conn db if possible... XXX
          activate-type-selector? (r/atom false)
          selected-type (r/atom nil)
          cancel-fn (fn []
                      (reset! activate-type-selector? false)
                      (reset! selected-type nil)
                      false)
          ok-fn (fn []
                  (reset! activate-type-selector? false)
                  (create-type-reference app eid attr-ident @selected-type)
                  (reset! selected-type nil)
                  false)]
      ;; TODO Need to add sorting functionality here...
      (fn [app [_ context] [eid attr-ident value]]
        (let [pull-expr (::pull-expr context)
              conn (:conn app)
              eid (:db/id @(safe-pull conn '[*] eid))
              value (or value (get @(safe-pull conn [attr-ident] eid) attr-ident))
              local-context (:dat.view.context/locals context)]
          ;; Ug... can't get around having to duplicate :field and label-view
          (when (and eid
                     (not (or (:attribute/hidden? context) (= :db/id attr-ident))))
            (let [type-idents (:attribute.ref/types @attr-sig)]
              ;; Are controls still separated this way? Should they be?
              [:div (:dom/attrs context)
               [field-for-skeleton app attr-ident
                ;; Right now these can't "move" because they don't have keys XXX Should fix with another component
                ;; nesting...
                ;; All of these things should be rewritten in terms of controls, and controls should be more cleanly separated out in context XXX
                [(when (= :db.cardinality/many (:db/cardinality @attr-sig))
                   ^{:key (hash :add-reference-button)}
                   [add-reference-button (fn []
                                           (cond
                                             (> (count type-idents) 1)
                                             (reset! activate-type-selector? true)
                                             ;; Should specifically catch this and let user select from any possible type; or maybe a defaults? context?
                                             (= (count type-idents) 0)
                                             (js/alert "No types associated with this attribute; This will be allowed in the future, till then please find/file a GH issue to show interest.")
                                             :else
                                             (create-type-reference app eid attr-ident (first type-idents))))])
                 ;; Need a flexible way of specifying which attributes need special functions associated in form
                 (when @activate-type-selector?
                   ^{:key (hash :attr-type-selector)}
                   [re-com/modal-panel
                    :child [attr-type-selector type-idents selected-type ok-fn cancel-fn]])]
                ;; Then for the actual value...
                ;(for [value (or (seq (utils/deref-or-value value)) [nil])]
                (for [value (let [value (utils/deref-or-value value)]
                              (or
                                (and (sequential? value) (seq value))
                                (and value [value])
                                [nil]))]
                  ^{:key (hash {:component :field-for :eid eid :attr-ident attr-ident :value value})}
                  [represent app [::input-for local-context] [eid attr-ident value]])]])))))))
                  ;[input-for app context-data pull-expr eid attr-ident value])]])))))))

;; TODO Need to rewrite with saner arity
(defn field-for
  [app context pull-expr eid attr-ident value]
  (let [context (assoc context ::pull-expr pull-expr)]
    [represent app [::fields-for context] [eid attr-ident value]]))


(defn get-remote-eid
  [app eid]
  (:datsync.remote.db/id (d/pull @(:conn app) [:datsync.remote.db/id] eid)))

(defn delete-entity-handler
  [app eid]
  (when (js/confirm "Delete entity?")
    (let [entity (d/pull @(:conn app) [:e/type :datsync.remote.db/id] eid)]
      (js/console.log (str "Deleting entity: " eid))
      (match [entity]
        ;; may need the ability to dispatch in here;
        :else
        (send-tx! app [[:db.fn/retractEntity eid]])))))


;(defn pull-expression-context
;  [pull-expr]
;  ;; Have to get this to recursively pull out metadata from reference attributes, and nest it according to context schema XXX
;  (meta pull-expr))

(defn rest-attributes
  "Grabs attributes corresponding to * pulls, not otherwise fetched at the top level of a pull-expr"
  ;; Is this something we should cache?
  [pull-expr pull-data]
  (->> pull-expr
       (map (fn [attr-spec]
              (if (map? attr-spec)
                (keys attr-spec)
                attr-spec)))
       flatten
       (remove (keys pull-data))))


;; The following three functions are currently not being used, and I'm not sure if they need to be. They overlap with
;; the `pull-attributes` function.

;(defn pull-expr-attributes
;  ;; TODO Take into account hidden
;  [app pull-expr]
;  (->> pull-expr
;       (map (fn [x] (if (map? x) (keys x) x)))
;       ;; If we keep this...
;       ;(remove #{'*})
;       flatten
;       distinct))
;
;
;(defn pull-with-extra-fields
;  ([pull-expr extra-fields]
;   (distinct
;     (concat
;       (map
;         (fn [attr-spec] (if (map? attr-spec)
;                           (into {} (map (fn [k pull-expr']
;                                           [k (pull-with-extra-fields pull-expr' extra-fields)])))))
;         pull-expr)
;       extra-fields)))
;  ([pull-expr]
;    ;; Need to be able to nest in type ident...
;   (pull-with-extra-fields pull-expr [:db/id :db/ident :e/type])))
;
;
;(defn pull-attr-values
;  [app pull-expr pull-data]
;  (->> (pull-expr-attributes app pull-expr)
;       (concat (keys pull-data))
;       (distinct)
;       (map (fn [attr-ident] [attr-ident (get pull-data attr-ident)]))
;       (into {})))


(declare type-pull)
(declare entity-pull)

(representation/register-representation
  ::pull-form
  (fn [app [_ context] [pull-expr pull-data-or-id]]
    (let [pull-data-or-id (utils/deref-or-value pull-data-or-id)
          local-context (:dat.view.context/locals context)
          eid (cond
                ;; id or lookup ref
                ((some-fn integer? vector?) pull-data-or-id) pull-data-or-id
                ;; presumably, data returned from said query
                (map? pull-data-or-id) (:db/id pull-data-or-id))]
      (cond
        ; again, id or lookup ref
        ((some-fn integer? vector?) pull-data-or-id)
        (let [pull-data (safe-pull (:conn app) pull-expr pull-data-or-id)]
          [represent app [::pull-form local-context] [pull-expr pull-data]])
        ; again, id or lookup ref
        (map? pull-data-or-id)
        [:div (:dom/attrs context)
         ;(for [[attr-ident values] (pull-attr-values app pull-expr pull-data-or-id)]
         (for [attr-ident (pull-attributes pull-expr pull-data-or-id)]
           (let [values (get pull-data-or-id attr-ident)]
             ^{:key (hash attr-ident)}
             ;; Need to assoc in the attr-ident to context as well, so the correct context can be prepared for the child representation
             [represent app [::fields-for (assoc local-context :db.attr/ident attr-ident)] [eid attr-ident values]]))]))))

(defn pull-form
  "Renders a form with defaults from pull data, or for an existing entity, subject to optional specification of a
  pull expression (possibly annotated with context metadata), a context map"
  ;; How to make this language context based...
  ([app pull-data-or-eid]
   [pull-form app @(entity-pull app pull-data-or-eid) pull-data-or-eid])
  ([app pull-expr pull-data-or-eid]
   (pull-form app (meta-context pull-expr) pull-expr pull-data-or-eid))
  ([app context pull-expr pull-data-or-eid]
   (when pull-data-or-eid
     [represent app [::pull-form context] [pull-expr pull-data-or-eid]])))


(defn edit-entity-form
  [app eid]
  (if-let [eid @(pull-attr (:conn app) eid :db/id)]
    (let [pull-expr @(entity-pull app eid)]
      [re-com/v-box :children [;[debug "Pull data:" @(posh/pull (:conn app) '[* {:e/type [*]}] eid)]
                               [:h3 "Editing " [pull-summary-string app @(posh/pull (:conn app) pull-expr eid)]]
                               [pull-form app eid]
                               [pull-view app pull-expr eid]]])
    [loading-notification "Please wait; form data is loading."]))


;; These are our new goals

;(defn pull-data-form
;  [app pull-expr eid]
;  (if-let [current-data @(safe-pull (:conn app) pull-expr eid)]
;    [re-com/v-box :children [[pull-form app pull-expr eid]]]
;    [loading-notification "Please wait; loading data."]))

;(defn pull-form
;[app pull-expr eid])



;; ## Constructing queries with metadata annotations


(defn type-data
  [app base-type]
  (safe-pull
    (:conn app)
    '[:db/id :db/ident :db/isComponent
      {:e/type ...
       :e.type/isa ...
       :e.type/attributes ...
       :db/valueType ...
       :attribute.ref/types ...}]
    base-type))


;; XXX Note; recursive isComponent attribute relations break this
(def type-pull
  (memoize
    (fn type-pull*
      ([app base-type]
       (type-pull* app {} base-type))
      ([app context base-type]
       (reaction
         (let [type-data @(type-data app base-type)]
           (walk/postwalk
             (fn [data]
               (cond
                 ;; For types
                 (:e.type/attributes data)
                 (->> (:e.type/attributes data)
                      ;; Assoc in a virtual attribute about whether a ref or not
                      (map (fn [attr] (assoc attr :db.type/ref? (-> attr :db/valueType :db/ident #{:db.type/ref}))))
                      ;; Mocking in :db/id, :db/ident and :e/type, since want for everything
                      (concat [{:db/ident :db/id}
                               ;; Should hide ident if not needed TODO
                               {:db/ident :db/ident}
                               {:db/ident :e/type
                                :db.type/ref? true
                                :attribute.ref/types [{:db/ident :e.type/Type
                                                       :e.type/attributes [{:db/ident :db/id}
                                                                           {:db/ident :db/ident}]}]}])
                      (sort-by (fn [attr] (or (:e/order attr)
                                              (cond
                                                (:db/isComponent attr) 2
                                                (:db.type/ref? attr) 1
                                                :else 0))))
                      ;; TODO Take into account isa subtypes and super types
                      (map (fn [attr]
                             (if (:db.type/ref? attr)
                               (if (:db/isComponent attr)
                                 {(:db/ident attr)
                                  (with-meta
                                    (->> (:attribute.ref/types attr)
                                         flatten
                                         (remove nil?)
                                         ;; Note; I guess we don't need pull extras here since *
                                         (concat ['*])
                                         vec)
                                    {:ref true})}
                                 {(:db/ident attr)
                                  ;; TODO Handle these
                                  (-> (::pull-summary-attrs context)
                                      (get (:db/ident attr))
                                      (concat [:e/name :e/description :db/ident {:e/type [:db/id :db/ident]}])
                                      vec
                                      (with-meta {;::representation ::pull-summary-view
                                                  ::collapsed? true ::collapsable? true}))})
                               (:db/ident attr))))
                      ;; Oh... shouldn't need this. This was probably because of the component refs?
                      (remove nil?)
                      distinct
                      vec
                      (#(with-meta % (merge (meta %) {;:e/type data
                                                      :e/type-ident (:db/ident data)}))))
                 :else data))
             type-data)))))))


(def entity-pull
  ;(memoize
    (fn entity-pull*
      [app entity-or-eid]
      (cond
        ;; If a map, use id to defer to else case  TODO could look here for type ids first...
        (map? entity-or-eid)
        (entity-pull* app (:db/id entity-or-eid))
        ;; This is where all the real logic is:
        :else ;; assume eid
        (let [type-id-rx (pull-path (:conn app) entity-or-eid [:e/type :db/id])]
          (reaction
            (if-let [type-id @type-id-rx]
              @(type-pull app type-id)
              (do
                (log/warn "Bad type id for entity-or-eid: " entity-or-eid)
                base-pull)))))))


;; This is effectively our metadata model

;(s/def ::pull-kv
;  ;; Should make this a recursive thing that fully specs...
;  (s/cat :reference keyword? :pull-expr vector?))

;(s/def ::pull-expr
;  (s/* (s/or keyword? map? symbol?)))

(defn meta-context
  [pull-expr]
  (let [ref-attrs (filter map? pull-expr)
        non-ref-attrs (remove map? pull-expr)]
    (assoc
      (meta pull-expr)
      ::pull-expr pull-expr
      ::ref-attrs
      (->> ref-attrs
           (apply merge)
           (map
             (fn [[attr-ident attr-pull-expr]]
               [attr-ident (meta-context attr-pull-expr)]))
           (into {}))
      ::non-ref-attrs non-ref-attrs)))


;; TODO Need a clear-metadata function as well, for clearing out the extracted metadata


;; Setting default context; Comes in precedence even before the DS context
;; But should this be config technically?

;; TODO A datalog model for context?: (would be nice to move towards this)

;; :e/type
;;   :e.type/Context
;; ::context
;; ::ident (:dat.view/context-id?)
;;   :context-id / whatevs
;; :dat.view.context/level
;;   :dat.view.context.level/entity
;;   :dat.view.context.level/attribute
;; :dat.view.context/attribute
;; :dat.view.context/type
;; :dat.view.context/type

;; :dom/attrs
;; ::controls
;; ::middleware
;; ::delegate-to


(swap! context/default-base-context
  utils/deep-merge
  ;; Top level just says that this is our configuration? Or is that not necessary?
  {::base-config
   {::pull-form
    {:dom/attrs {:style bordered-box-style}}
    ::attr-values-view
    {:dom/attrs {:style h-box-styles}
     ;; Right now only cardinality many attributes are collapsable; Should be able to set any? Then set for cardinality many as a default? XXX
     ::collapsable? true
     ::collapsed? true} ;; Default; everything is collapsed
    ::value-view
    {:dom/attrs {:style (merge h-box-styles
                               {:padding "3px"})}}
    ::attr-view
    {:dom/attrs {:style (merge v-box-styles
                               {:padding "5px 12px"})}}
    ::label-view
    {:dom/attrs {:style {:font-size "14px"
                         :font-weight "bold"}}}
    ::pull-data-view
    {:dom/attrs {:style (merge h-box-styles
                               bordered-box-style
                               {:padding "8px 15px"
                                :width "100%"})}}
     ;; Hmm... maybe this should point to the keyword so it can grab from there?
     ;::summary pull-summary-view
     ;::component pull-view}
    ;; XXX This should change shortly...
    ::pull-view-controls
    {:dom/attrs {:style (merge h-box-styles
                               {:padding "5px"})}}
                                ;:background "#DADADA"})}
                                ;;; Check if these actually make sense
                                ;:justify-content "flex-end"})}}
                                ;:gap "10px"
     ;::component default-pull-view-controls}
    ::pull-summary-view
    {:dom/attrs {:style (merge v-box-styles
                               {:padding "15px"
                                :font-size "18px"
                                :font-weight "bold"})}}
     ;::component pull-summary-view}
    ::field-for
    {:dom/attrs {:style v-box-styles}}}
   ;; Specifications merged in for any config with a certain cardinality
   ::card-config {}
   ;; Specifications merged in for any value type
   ::value-type-config {}
   ::attr-config {:db/id {::field-for {:attribute/hidden? true
                                       :dom/attrs {:style {:display "none"}}}}
                  :db/ident {::field-for {:attribute/hidden? true
                                          :dom/attrs {:style {:dispay "none"}}}}}})
   ;; Will add the ability to add mappings at the entity level; And perhaps specifically at the type level.
   ;; Use the patterns of OO/types with pure data; Dynamic

;; ## History & Routing
;; ====================

;; Realy need to set this one up as a component, but for now...

;; Start watching history and on changes, set the :dat.view/route attribute of the conn db
(comment
  (defonce history
    (let [conn (-> system :app :conn) ;; Should probably base this off app directly once component
          history-obj (doto (router/make-history)
                        (router/attach-history-handler! (router/make-handler-fn conn)))]
      (settings/update-setting conn :dat.view/history-obj history-obj)
      ;; Initialize route, really; we don't have a :dat.view/route set in the db yet, so need to instantiate
      (router/update-route! conn))))



;; Here's where everything comes together
;; Datview record instances are what we pass along to our Datview component functions as the first argument.
;; Abstractly, they are just a container for your database and communications functionality (via attributes :conn and :config).
;; But in reality, they are actually Stuart Sierra components, with start and stop methods.
;; You can either use these components standalone, by creating your app instance with `(new-datview ...)`, and starting it with the `start` function (both defined below).
;; Convention is to call datview instances either app or datview.
;; But you should be thinking about them as the application object of your program.

;; Should make this derefable

(defrecord Datview 
  ;;  The public API: these two attributes
  [conn   ;; You can access this for your posh queries; based on reactor unless otherwise specified
   config ;; How you control the instantiation of Datview; options:
   routes ;; Bidi routes data (will abstract more eventually)
   ;; * :datascript/schema
   ;; * :dat.view/conn
   ;; Other (semi-)optional dependencies
   remote  ;; Something implementing the dat.remote protocols; If not specified as a dependency, fetches from reactor
   dispatcher ;; Something implementing the dispatcher protocols
   main] ;; Need to make this a clear requirement
  component/Lifecycle
  (start [component]
    (try
      (log/info "Starting Datview")
      ;; TODO Ugg... need to have a way for Datsync to register its default schema
      (let [base-schema (utils/deep-merge {:db/ident {:db/ident :db/ident :db/unique :db.unique/identity}}
                                          (:datascript/schema config))
            ;; Should try switching to r/atom
            ;conn (or conn (::conn config) (r/atom (d/empty-db base-schema)))
            conn (or conn (::conn config) (d/create-conn base-schema))
            routes (or routes (::routes config) routes/routes) ;; base routes
            main (or main (::main config))
            history (router/make-history)
            component (assoc component :conn conn :main main :history history :routes routes)]
        ;; Transact default settings to db
        (d/transact! conn default-settings)
        ;; Start posh
        (posh/posh! conn)
        ;; Install settings entity
        (settings/init! component)
        ;; TODO Fire off the router handlers
        (router/attach-history-handler! history (router/make-handler-fn component))
        component)
      (catch :default e
        (log/error "Error starting Datview:" e)
        (println (.-stack e))
        component)))
  (stop [component]
    (assoc component
           :reactor nil
           :conn nil)))



;; Should have a way of telling components what config options they need
(defn new-datview
  "Creates a new instance of datview, to be passed around in your application code as either
  `app` or `datview` (the latter, following from typical System Component naming conventions,
  and the fact that this will be a Datview object)"
  ([{:as config
     :keys [datascript/schema ;; Base schema
            dat.view/conn
            dat.view/base-context]
     :or {dat.view/base-context default-base-context}}] ;; Need to actually plug this in as an atom
   (map->Datview {:config config}))
  ([]
   (new-datview {})))


