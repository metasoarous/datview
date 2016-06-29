(ns dat.view
  "# Datview"
  (:require-macros [reagent.ratom :refer [reaction]]
                   [cljs.core.async.macros :as async-macros :refer [go go-loop]])
  (:require [dat.reactor :as reactor]
            [dat.reactor.dispatcher :as dispatcher]
            [dat.view.router :as router]
            [dat.view.utils :as utils]
            [dat.spec.protocols :as protocols]
            [datascript.core :as d]
            [posh.core :as posh]
            [reagent.core :as r]
            [reagent.ratom :as ratom]
            [re-com.core :as re-com]
            [taoensso.timbre :as log :include-macros true]
            [com.stuartsierra.component :as component]
            [goog.date.Date]
            [cljs-time.core :as cljs-time]
            [cljs.core.async :as async]
            [cljs.spec :as s]
            [cljs-time.format]
            [cljs-time.coerce]
            [cljs.pprint :as pp]
            [cljs.core.match :as match :refer-macros [match]]
    #_[markdown.core :as md]
      [dat.view.query :as query]))





;; ## Represent

;; This is really the cornerstone of all of dat.view
;; This multimethod represents the abstract ability to render/represent something based on abstract context

(defmulti represent
  (fn [app [context-id context-data] data]
    context-id))


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

(defn send-tx!
  "Helper function for dispatching tx messages to server."
  [app tx-forms]
  ;; TODO This should be smarter, and look to see whether dat.sys is loaded, and dispatch occordingly
  (dispatch! app [:dat.sync.client/send-remote-tx tx-forms]))


(defn send-remote-event!
  [app remote-event]
  (dispatch! app [:dat.remote/send-event! remote-event]))



;; ## Metadata view specification structure defaults

(def ^:dynamic box-styles
  {:display "inline-flex"
   :flex-wrap "wrap"})

(def ^:dynamic h-box-styles
  (merge box-styles
         {:flex-direction "row"}))

(def ^:dynamic v-box-styles
  (merge box-styles
         {:flex-direction "column"}))

(def bordered-box-style
  {:border "2px solid grey"
   :margin "3px"
   :background-color "#E5FFF6"})

(def default-pull-data-view-style
  (merge h-box-styles
         {:padding "8px 15px"
          :width "100%"}))

(def default-attr-view-style
  (merge v-box-styles
         {:padding "5px 12px"}))


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
    [:pre (debug-str data)]])
  ([data]
   (debug "" data)))



;; ## Context

;; We're going to be re-describing things in terms of context.
;; Context includes configuration and contextual information about where things are.
;; But it is extensible, so we can pass through whatever information we might like about how to render things.

;; All of these should be checked for their semantics on :dat.view.base-context/value etc; Is this the right way to represent these things?

;; Should probably move all of these out to reactions or some such, except for anything that's considered public

(defonce default-base-context (r/atom {}))

(def base-context
  ;; Not sure if this memoize will do what I'm hoping it does (:staying-alive true, effectively)
  (memoize
    (fn [app]
      ;; Hmm... should we just serialize the structure fully?
      ;; Adds complexity around wanting to have namespaced attribute names for everything
      (reaction
        (try
          (:dat.view.base-context/value
            @(posh/pull (:conn app) '[*] [:db/ident ::base-context]))
          ;; Easter egg:
          ;; A self installing config entity :-) Good pattern?
          (catch :default e
            (log/warn "You don't yet have a :dat.view/base-context setting defined. Creating one.")
            (dispatch! app [:dat.reactor/local-tx [{:db/ident ::base-context}]])))))))

(defn update-base-context!
  [app f & args]
  (letfn [(txf [db]
            (apply update
                   (d/pull db '[*] [:db/ident ::base-context])
                   :dat.view.base-context/value
                   f
                   args))]
    (d/transact! (:conn app) [[:db.fn/call txf]])))

(defn set-base-context!
  [app context]
  (update-base-context! app (constantly context)))



;; ## Reactions

(defn as-reaction
  "Treat a regular atom as though it were a reaction"
  [vanilla-atom]
  (let [trigger (r/atom 0)]
    (add-watch vanilla-atom :as-reaction-trigger (fn [& args] (swap! trigger inc)))
    (reaction
      @trigger
      @vanilla-atom)))

;; XXX This will be coming to posh soon, but in case we need it earlier

(defn pull-many
  [app pattern eids]
  (let [conn-reaction (as-reaction (:conn app))]
    (reaction (d/pull-many @conn-reaction pattern eids))))


(defn meta-sig
  [args-vec]
  (mapv #(vector % (meta %)) args-vec))

(defn meta-memoize
  ([f]
   ;; Don't know if this actually has to be an r/atom; may be more performant for it not to be
   (meta-memoize f (r/atom {})))
  ([f cache]
   (fn [& args]
     (if-let [cached-val (get @cache (meta-sig args))] 
       cached-val
       (let [new-val (apply f args)]
         (swap! cache assoc (meta-sig args) new-val)
         new-val)))))

;; ### Attribute metadata reactions

(def attribute-schema-reaction
  "Returns the corresponding attr-ident entry from the Datomic schema. Returns full entity references; Have to path for idents."
  (memoize
    (fn [app attr-ident]
      (if (= attr-ident :db/id)
        (reaction {:db/id nil})
        (posh/pull (:conn app)
                   '[* {:db/valueType [:db/ident]
                        :db/cardinality [:db/ident]
                        :attribute.ref/types [:db/ident]}]
                   [:db/ident attr-ident])))))

;; Another function gives us a version of this that maps properly to idents
(def attribute-signature-reaction
  "Reaction of the pull of a schema attribute, where any references to something with any ident entity
  have been replaced by that ident keyword."
  (memoize
    (fn [app attr-ident]
      (let [schema-rx (attribute-schema-reaction app attr-ident)]
        (reaction
          (into {}
            (letfn [(mapper [x]
                      (or (:db/ident x)
                          (and (sequential? x) (map mapper x))
                          x))]
              (map (fn [[k v]] [k (mapper v)])
                   @schema-rx))))))))


;; This is what does all the work of computing our context for each component

(def component-context
  "This function returns the component configuration (base-context; should rename) for either an entire render network,
  abstractly, or for a specific component based on a component id (namespaced keyword matching the function to be called)."
  (memoize
    (fn component-context*
      ([app]
       (reaction
         ;; Don't need this arity if we drop the distinction between base-context and default-base-context
         @default-base-context
         (utils/deep-merge
           @(base-context app))))
      ([app component-id]
       (component-context* app component-id {}))
      ([app component-id {;; Options, in order of precedence in consequent merging
                          :keys [dat.view/locals ;; points to local overrides; highest precedence
                                 ;; When the component is in a scope closed over by some particular attribute:
                                 dat.view/attr]}] ;; db/ident of the attribute; precedence below locals
       (reaction
         (let [merged (utils/deep-merge @(component-context app) (utils/deref-or-value locals))]
           (if attr
             (let [attr-sig @(attribute-signature-reaction app attr)]
               (utils/deep-merge (get-in merged [::base-config component-id])
                                 (get-in merged [::card-config (:db/cardinality attr-sig) component-id])
                                 (get-in merged [::value-type-config (:db/valueType attr-sig) component-id])
                                 (get-in merged [::attr-config attr component-id])))
             ;; Need to also get the value type and card config by the attr-config if that's all that's present; Shouldn't ever
             ;; really need to pass in manually XXX
             (get-in merged [::base-config component-id]))))))))



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


(defmethod represent ::pull-summary-string
  [_ _ pull-data]
  (match [pull-data]
    [{:e/name name}] name
    [{:e/type {:db/ident type-ident}}] (name type-ident)
    [{:attribute/label label}] label
    ;; A terrible assumption really, but fine enough for now
    :else (pr-str pull-data)))

(defn pull-summary-string
  [app context-data pull-data]
  (represent app [::pull-summary-string context-data] pull-data))


(defmethod represent :pull-summary-view
  [app [_ context-data] pull-data]
  [:div {:style {:font-weight "bold" :padding "5px"}}
   (represent app [::pull-summary context-data] pull-data)])

(defn pull-summary-view
  [app context-data pull-data]
  (represent app [::pull-summary-view context-data] pull-data))


(defmethod represent ::collapse-summary
  [app [_ context-data] values]
  ;; XXX Need to stylyze and take out of re-com styling
  [:div {:style (merge v-box-styles
                       {:padding "10px"})}
                        ;:align :end
                        ;:gap "20px"
   (for [value (distinct values)]
     ^{:key (hash value)}
     [pull-summary-view app context-data value])])

(defn collapse-summary
  [app context-data values]
  (represent app [::collapse-summary context-data] values))



;; ## Attribute view

;; View all of the values for some entity, attribute pair
;; Values must be passed in explicitly, or in an atom

(defn lablify-attr-ident
  [attr-ident]
  (let [[x & xs] (clojure.string/split (name attr-ident) #"-")]
    (clojure.string/join " " (concat [(clojure.string/capitalize x)] xs))))


(defmethod represent ::label-view
  [app _ attr-ident]
  (when attr-ident
    [re-com/label
     :style {:font-size "14px"
             :font-weight "bold"}
     :label
     ;; XXX Again, should be pull-based
     (or (:attribute/label @(posh/pull (:conn app) [:attribute/label] [:db/ident attr-ident]))
         (lablify-attr-ident attr-ident))]))

(defn label-view
  "For a given attr-ident, render a label for that attribute."
  ([app context-data attr-ident]
   (represent app [::label-view context-data] attr-ident))
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


(declare pull-data-view)


;; Still need to hook up with customized context

(defmethod represent ::copy-entity-control
  [app [_ context-data] pull-data]
  (let [pull-data (utils/deref-or-value pull-data)]
    ;; TODO Need to figure out the right way to configure the re-com components
    [re-com/md-icon-button :md-icon-name "zmdi-copy"
     :size :smaller
     :style {:margin-right "10px"}
     :tooltip "Copy entity" ;; XXX Should make tooltip fn-able
     :on-click (fn [] (js/alert "Coming soon to a database application near you"))]))

(defmethod represent ::edit-entity-control
  [app [_ context-data] pull-data]
  (let [pull-data (utils/deref-or-value pull-data)]
    [re-com/md-icon-button :md-icon-name "zmdi-edit"
     :style {:margin-right "10px"}
     :size :smaller
     :tooltip "Edit entity"
     ;; This assumes the pull has :dat.sync.remote.db/id... automate?
     :on-click (fn [] (router/set-route! app {:handler :edit-entity :route-params {:db/id (:dat.sync.remote.db/id pull-data)}}))]))


;; TODO Need a way to figure out which controls are needed for a given component
(defn component-controls
  [context-data]
  ;; For now..
  (::controls context-data))

(defmethod represent ::control-set
  [app [_ context-data] data]
  (let [context-data (component-context app ::control-set {::locals context-data})]
    ;; XXX This was ::pull-view-controls, now ::control-set
    [:div (:dom/attrs @(component-context app ::pull-view-controls {::locals context-data}))
     (for [control-id (distinct (component-controls context-data))]
       ^{:key (hash control-id)}
       [represent app [control-id context-data] data])]))



(defn default-pull-view-controls
  [app context-data pull-data]
  (let [context [::controls (assoc context-data ::controls [::copy-entity-control ::edit-entity-control])]]
    (represent app context pull-data)))

(defn default-field-for-controls
  [app pull-expr pull-data]
  (let [context (component-context app ::default-field-for-controls {::locals (meta pull-expr)})]
    [:div (:dom/attrs context)])) 


;; Hmm... decided I don't like the [attr-ident value] thing; attr-ident should really be part of the context.
;; If not present, should just default.
;; Maybe slightly less than ideal for refs, but still valueable I think.
;; Attr ident is really just context, which we may or may not need.
(defmethod represent ::value-view
  ;; QUESTION Should attr-ident be part of the context-data?
  [app [_ context-data] value]
  (let [attr-ident (:db.attr/ident context-data)
        attr-sig @(attribute-signature-reaction app attr-ident)
        context @(component-context app ::value-view {::locals context-data})]
    [:div (:dom/attrs context)
     ;[debug "Here is the comp-attrs:" attr-sig]
     (match [attr-sig]
       ;; For now, all refs render the same; May treat component vs non-comp separately later
       [{:db/valueType :db.type/ref}]
       (let [nested-context (assoc context-data ::pull-expr (get-nested-pull-expr (::pull-expr context-data) attr-ident))]
         ;; QUESTION: Where should the nsted pull-expr go?
         [represent app nested-context value])
       ;; Miscellaneous value
       :else
       (str value))]))


;; TODO Need to figure out the signature here
;(defn value-view
;  [app pull-expr attr-ident value]



;; Should we have a macro for building these components and dealing with all the state in the context? Did the merge for you?
;(defn build-view-component)

(defmethod represent ::attr-values-view
  [app [_ context-data] values]
  (let [pull-expr (::pull-expr context-data)
        context @(component-context app ::attr-values-view {::locals (meta pull-expr)})
        ;; Should put all of the collapsed values in something we can serialize, so we always know what's collapsed
        collapse-attribute? (r/atom (:dat.view.collapse/default context))]
    (fn [app [_ context-data] values]
      (let [collapsable? (:dat.view.collapse/collapsable? context)]
        [:div (:dom/attrs context)
         (when collapsable?
           [collapse-button collapse-attribute?])
         (when @collapse-attribute?
           [collapse-summary app context values])
         ;(defn pull-summary-view [app pull-expr pull-data]
         (when (or (not collapsable?) (and collapsable? (not @collapse-attribute?)))
           (for [value (utils/deref-or-value values)]
             ^{:key (hash value)}
             [represent app context-data value]))]))))


;(defn attr-values-view
;  [app context attr-ident values])


;; Need to have controls etc here
(defmethod represent ::attr-view
  [app [_ context-data] values]
  (let [attr-ident (:db.attr/ident context-data)]
    [:div (:dom/attrs @(component-context app ::attr-view {:dat.view/locals context-data}))
    ;[:div @(attribute-context app (meta pull-expr) :attr-view)
     [label-view app attr-ident]
     (match [@(attribute-signature-reaction app attr-ident)]
       [{:db/cardinality :db.cardinality/many}]
       [represent app [::attr-values-view context-data] [attr-ident values]]
       :else
       [represent app [::value-view context-data] [attr-ident values]])]))


;(defn attr-view
;  [app pull-expr attr-ident values])


;; All rendering modes should be controllable via registered toggles or fn assignments
;; registration modules for plugins
;; * middleware?

(defn pull-attributes
  ([pull-expr pull-data]
   (->> pull-expr
        (map (fn [attr-spec]
               (cond
                 (keyword? attr-spec) attr-spec
                 (map? attr-spec) (keys attr-spec)
                 (symbol? attr-spec)
                 (case attr-spec
                   '* (filter
                        (set (pull-attributes (remove #{'*} pull-expr) []))
                        (keys (utils/deref-or-value pull-data)))))))
        flatten
        distinct))
  ([pull-expr]
   (pull-attributes pull-expr [])))


(defmethod represent ::pull-data-view
  [app [_ context-data] pull-data]
  ;; Annoying to have to do this
  (let [context @(component-context app ::pull-data-view {:dat.view/locals context-data})
        pull-expr (::pull-expr context)
        pull-data (utils/deref-or-value pull-data)]
    [:div (:dom/attrs context)
     [:div
       [represent app [::control-set context-data] pull-data]
       [:div {:style (merge h-box-styles)}
        [represent app [::pull-summary-view context-data] pull-data]]]
     ;; XXX TODO Questions:
     ;; Need a react-id function that lets us repeat attrs when needed
     (for [attr-ident (distinct (pull-attributes pull-expr pull-data))]
       ^{:key (hash attr-ident)}
       [represent app [::attr-view context-data] [attr-ident (get pull-data attr-ident)]])]))


(defmethod represent ::pull-view
  [app [_ context-data] [pull-expr eid]]
  (let [pull-data (posh/pull (:conn app) pull-expr eid)]
    [represent app [::pull-data-view (assoc context-data ::pull-expr pull-expr)] pull-data]))


(defn pull-data-view
  "Given a DS connection, a app pull-expression and data from that pull expression (possibly as a reaction),
  render the UI subject to the pull-expr metadata."
  ;; Should be able to bind the data to the type dictated by pull expr
  ([app context-data pull-data]
   [represent app [::pull-data-view context-data] pull-data])
  ([app pull-data]
   (pull-data-view app {} pull-data)))

(defn pull-view
  ([app context-data pull-expr eid]
   [represent app [::pull-view context-data] [pull-expr eid]])
  ([app pull-expr eid]
   (pull-view app {} pull-expr eid)))

;; General purpose sortable collections in datomic/ds?
;; Should use :attribute/sort-by; default :db/id?


(defn attr-sort-by
  [app attr-ident]
  (reaction (or (:db/ident (:attribute/sort-by @(posh/pull (:conn app) '[*] [:db/ident attr-ident])))
                ;; Should add smarter option for :e/order as a generic? Or is this just bad semantics?
                :db/id)))

(defn value-type
  [app attr-ident]
  (reaction (:db/valueType @(posh/pull (:conn app) '[*] [:db/ident attr-ident]))))

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

;(declare pull-form)

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


(defn select-entity-input
  {:todo ["Finish..."
          "Create some attribute indicating what entity types are possible values; other rules?"]}
  ([app eid attr-ident value]
    ;; XXX value arg should be safe as a reaction here
   (let [;options @(attribute-signature-reaction app attr-ident)]
         options (->>
                   @(posh/q (:conn app)
                            '[:find [(pull ?eid [*]) ...]
                              :in $ ?attr
                              :where [?attr :attribute.ref/types ?type]
                              [?eid :e/type ?type]]
                            [:db/ident attr-ident])
                   ;; XXX Oh... should we call entity-name entity-label? Since we're really using as the label
                   ;; here?
                   (mapv (fn [pull-data] (assoc pull-data :label (pull-summary pull-data)
                                                          :id (:db/id pull-data))))
                   (sort-by :label))]
     ;; Remove this div; just for debug XXX
     [:div
      ;[debug "options:" options]
      [select-entity-input app eid attr-ident value options]]))
  ([app eid attr-ident value options]
   [re-com/single-dropdown
    :style {:min-width "150px"}
    :choices options
    :model (:db/id value)
    :on-change (partial apply-reference-change! app eid attr-ident (:db/id value))]))


;; Simple md (markdown) component; Not sure if we really need to include this in dat.view or not...
(defn md
  [md-string]
  [re-com/v-box
   :children
   [[:div {:dangerouslySetInnerHTML {:__html (md/md->html md-string)}}]]])


;; ### Datetimes...

;; TODO Need to get proper date+time handlers that handle both the time and the

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
    (fn []
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
    ;; XXX TODO Need to base this on the generalized stuff
   (let [attr @(attribute-signature-reaction app attr-ident)]
     (match [attr context]
            ;; The first two forms here have to be compbined and the decision about whether to do a dropdown
            ;; left as a matter of the context (at least for customization); For now leaving though... XXX
            ;; We have an isComponent ref; do nested form
            ;; Should this clause just be polymorphic on whether value is a map or not?
            [{:db/valueType :db.type/ref :db/isComponent true} _]
            ;; Need to assoc in the root node context here
            (let [sub-expr (some #(get % attr-ident) pull-expr) ;; XXX This may not handle a ref not in {}
                  ;; Need to handle situation of a recur point ('...) as a specification; Should be the context pull root, or the passed in expr, if needed
                  sub-expr (if (= sub-expr '...) (or (:dat.view/root-pull-expr context) pull-expr) sub-expr)
                  context (if (:dat.view/root-pull-expr context)
                            context
                            (assoc context :dat.view/root-pull-expr pull-expr))]
              ;(when-not (= (:db/cardinality attr) :db.cardinality/many)
              ;;(nil? value))
              [pull-form app context sub-expr value])
            ;; This is where we can insert something that catches certain things and handles them separately, depending on context
            ;[{:db/valueType :db.type/ref} {:dat.view.level/attr {?}}]
            ;[pull-form app context (get pull-expr value)]
            ;; Non component entity; Do dropdown select...
            [{:db/valueType :db.type/ref} _]
            [select-entity-input app eid attr-ident value]
            ;; Need separate handling of datetimes
            [{:db/valueType :db.type/instant} _]
            [datetime-selector app eid attr-ident value]
            ;; Booleans should be check boxes
            [{:db/valueType :db.type/boolean} _]
            [boolean-selector app eid attr-ident value]
            ;; For numeric inputs, want to style a little differently
            [{:db/valueType (:or :db.type/float :db.type/double :db.type/integer :db.type/long)} _]
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
(defn field-for
  [app context pull-expr eid attr-ident value]
  ;; So first we get attr-signature and config
  (let [attr-sig (attribute-signature-reaction app attr-ident)
        config (component-context app ::field-for {:dat.view/locals context :dat.view/attr attr-ident})
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
    ;; XXX Need to add sorting functionality here...
    (fn [app context pull-expr eid attr-ident value]
      ;; Ug... can't get around having to duplicate :field and label-view
      (when (and @(posh/q (:conn app) '[:find ?eid :in $ ?eid :where [?eid]])
                 (not (:attribute/hidden? @config)))
        (let [type-idents (:attribute.ref/types @attr-sig)]
          ;; Are controls still separated this way? Should they be? XXX
          [:div (:dom/attrs @config)
           ;[debug "type-idents:" type-idents]
           ;[debug "attr-sig:" @attr-sig]
           ;[:div (get-in @config [:dat.view.level/attr :dat.view/controls])]
           [field-for-skeleton app attr-ident
            ;; Right now these can't "move" because they don't have keys XXX Should fix with another component
            ;; nesting...
            ;; All of these things should be rewritten in terms of controls, and controls should be more cleanly separated out in config XXX
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
              [:div
               ;[debug "value:" value]
               [input-for app context pull-expr eid attr-ident value]])]])))))

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


;; Let's do a thing where we have
(defn loading-notification
  [message]
  [re-com/v-box
   :style {:align-items "center"
           :justify-content "center"}
   :gap "15px"
   :children
   [[re-com/title :label message]
    [re-com/throbber :size :large]]])


(defn pull-expression-context
  [pull-expr]
  ;; Have to get this to recursively pull out metadata from reference attributes, and nest it according to context schema XXX
  (meta pull-expr))

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


(defn pull-expr-attributes
  [app pull-expr]
  (->> pull-expr
       (map (fn [x] (if (map? x) (keys x) x)))
       flatten
       distinct))


(defn pull-with-extra-fields
  ([pull-expr extra-fields]
   (distinct
     (concat
       (map
         (fn [attr-spec] (if (map? attr-spec)
                           (into {} (map (fn [k pull-expr']
                                           [k (pull-with-extra-fields pull-expr' extra-fields)])))))
         pull-expr)
       extra-fields)))
  ([pull-expr]
    ;; Need to be able to nest in type ident...
   (pull-with-extra-fields pull-expr [:db/id :db/ident :e/type])))


(defn pull-form
  "Renders a form with defaults from pull data, or for an existing entity, subject to optional specification of a
  pull expression (possibly annotated with context metadata), a context map"
  ;; How to make this language context based...
  ([app pull-data-or-eid]
   (pull-form app '[*] pull-data-or-eid))
  ([app pull-expr pull-data-or-eid]
   (pull-form app (pull-expression-context pull-expr) pull-expr pull-data-or-eid))
  ([app context pull-expr pull-data-or-eid]
   (when pull-data-or-eid
     (if (integer? pull-data-or-eid)
       (if-let [current-data @(posh/pull (:conn app) pull-expr pull-data-or-eid)]
         [pull-form app context pull-expr current-data]
         [loading-notification "Please wait; loading data."])
       ;; The meat of the logic
       (let [context @(component-context app ::pull-form {:dat.view/locals context})]
         [:div (:dom/attrs context)
          ;; Can you doubly nest for loops like this? XXX WARN
          (for [attr-ident (pull-expr-attributes app pull-expr)]
            ^{:key (hash attr-ident)}
            [field-for app context pull-expr (:db/id pull-data-or-eid) attr-ident (get pull-data-or-eid attr-ident)])])))))

;; We should use this to grab the pull expression for a given chunk of data
;(defn pull-expr-for-data

(defn edit-entity-form
  [app remote-eid]
  (if-let [eid @(posh/q (:conn app) '[:find ?e . :in $ ?remote-eid :where [?e :datsync.remote.db/id ?remote-eid]] remote-eid)]
    [re-com/v-box :children [[pull-data-form app eid]]]
    [loading-notification "Please wait; form data is loading."]))


;; These are our new goals

;(defn pull-data-form
;[app pull-expr eid]
;(if-let [current-data @(posh/pull (:conn app) pull-expr eid)]
;[re-com/v-box :children [[edit-entity-fieldset app eid]]]
;[loading-notification "Please wait; loading data."]))

;(defn pull-form
;[app pull-expr eid])


(swap! default-base-context
       utils/deep-merge
       ;; Top level just says that this is our configuration? Or is that not necessary?
       {:dat.view/base-config
                                    {::pull-form
                                     {:dom/attrs {:style bordered-box-style}}}
        ;; Specifications merged in for any config
        :dat.view/card-config {}
        ;; Specifications merged in for any value type
        :dat.view/value-type-config {}
        :dat.view/attr-config {}})




;; Setting default context; Comes in precedence even before the DS context
;; But should this be config technically?
;; Note: There are function values in here, so some of this would not be writable to Datomic; But at least some of it could be...)
;; This stuff should maybe be part of a transaction
(swap! default-base-context
  utils/deep-merge
  ;; Top level just says that this is our configuration? Or is that not necessary?
  {::base-config
   {::attr-values-view
    {:dom/attrs {:style h-box-styles}
     ;; Right now only cardinality many attributes are collapsable; Should be able to set any? Then set for cardinality many as a default? XXX
     :dat.view.collapse/collapsable? true
     :dat.view.collapse/default false} ;; Default; nothing is collapsed
    ::value-view
    {:dom/attrs {:style (merge h-box-styles
                               {:padding "3px"})}}
    ::attr-view
    {:dom/attrs {:style (merge v-box-styles
                               {:padding "5px 12px"})}}
    ::label-view
    {:dom/attrs {:style {:font-size "14px"
                         :font-weight "bold"}}
     ::component label-view}
    ::pull-data-view
    {:dom/attrs {:style (merge h-box-styles
                               bordered-box-style
                               {:padding "8px 15px"
                                :width "100%"})}
     ;; Hmm... maybe this should point to the keyword so it can grab from there?
     ::summary pull-summary-view
     ::component pull-view}
    ;; XXX This should change shortly...
    ::pull-view-controls
    {:dom/attrs {:style (merge h-box-styles
                               {:padding "5px"})}
                                ;:background "#DADADA"})}
                                ;;; Check if these actually make sense
                                ;:justify-content "flex-end"})}}
                                ;:gap "10px"
     ::component default-pull-view-controls}
    ::pull-summary-view
    {:dom/attrs {:style (merge v-box-styles
                               {:padding "15px"
                                :font-size "18px"
                                :font-weight "bold"})}
     ::component pull-summary-view}
    ::default-field-for-controls
    {::component default-field-for-controls}
    :dat.view.forms/field-for
    {:dom/attrs {:style v-box-styles}}}
   ;; Specifications merged in for any config
   ::card-config {}
   ;; Specifications merged in for any value type
   ::value-type-config {}
   ::attr-config {:db/id {:dat.view.forms/field-for {:attribute/hidden? true
                                                     :dom/attrs {:style {:display "none"}}}}}})
   ;; Will add the ability to add mappings at the entity level; And perhaps specifically at the type level.
   ;; Use the patterns of OO/types with pure data; Dynamic

;; ## History & Routing
;; ====================

;; Realy need to set this one up as a component, but for now...

;; Start watching history and on changes, set the :datview/route attribute of the conn db
(comment
  (defonce history
    (let [conn (-> system :app :conn) ;; Should probably base this off app directly once component
          history-obj (doto (router/make-history)
                        (router/attach-history-handler! (router/make-handler-fn conn)))]
      (settings/update-setting conn :datview/history-obj history-obj)
      ;; Initialize route, really; we don't have a :datview/route set in the db yet, so need to instantiate
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
            main (or main (::main config))
            history (router/make-history)
            component (assoc component :conn conn :main main :history history)]
        ;; Transact default settings to db
        (d/transact! conn default-settings)
        ;; Start posh
        (posh/posh! conn)
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


