(ns dat.view
  "# Datview"
  (:require-macros [reagent.ratom :refer [reaction]]
                   [cljs.core.async.macros :as async-macros :refer [go go-loop]])
  (:require [dat.reactor :as reactor]
            [dat.reactor.dispatcher :as dispatcher]
            [dat.view.router :as router]
            [dat.view.utils :as utils]
            [dat.spec.protocols :as protocols]
            [dat.sync.client :as dat.sync]
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
            #_[markdown.core :as md]))




(s/def ::event-id (s/and keyword? namespace))

(s/def ::event (s/and vector? (s/cat :event-id  ::event-id
                                     :event-data (constantly true))))
                                    
(s/def ::conn d/conn?)

(s/def ::dispatcher #(satisfies? protocols/PDispatcher %))

;;todo
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


(s/def ::dispatch-args (s/cat :app ::app  :event ::event :level (s/? keyword?)))


(s/fdef dispatch :args ::dispatch-args)



(defn dispatch-error!
  [app event]
  (dispatcher/dispatch-error! (:dispatcher app) event))


(s/fdef dispatch-error! :args (s/cat :app ::app :event ::event))



(defn send-tx! [app tx-forms]
  (dispatch! app [:dat.sync.client/send-remote-tx tx-forms]))








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
      ([app component-id {:as options
                           ;; Options, in order of precedence in consequent merging
                           :keys [dat.view/locals ;; points to local overrides; highest precedence
                                  ;; When the component is in a scope closed over by some particular attribute:
                                  dat.view/attr ;; db/ident of the attribute; precedence below locals
                                  dat.view/valueType ;; The :db/valueType of the attribute (as ident); lower precedence still
                                  dat.view/cardinality]}] ;; Cardinality (ident) of the value type; lowest precedence
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


;; ## Builder pieces

;; These are builder pieces part of the public api;
;; These should be accessible for wrapping, and should be overridable/extensible via correspondingly named keys of the context map at various entry points

(defn pull-summary
  [pull-data]
  (match [pull-data]
    [{:e/name name}] name
    [{:e/type {:db/ident type-ident}}] (name type-ident)
    [{:attribute/label label}] label
    ;; A terrible assumption really, but fine enough for now
    :else (pr-str pull-data)))

(defn pull-summary-view
  [app context pull-data]
  [:div {:style {:font-weight "bold" :padding "5px"}}
   (pull-summary pull-data)])

(defn collapse-summary
  [app context values]
  ;; XXX Need to stylyze and take out of re-com styling
  [:div {:style (merge v-box-styles
                       {:padding "10px"})}
                       ;:align :end
                       ;:gap "20px"
   (for [value values]
     ^{:key (hash value)}
     [pull-summary-view app context value])])

;; These summary things are still kinda butt ugly.
;; And they're something we need to generally spend more time on anyway.
;; Need to smooth out... XXX



;; ## Event handler

;; Need an even handler which can dispatch on some transaction patterns, and execute various messages or side effects.
;; I think posh may give this to us?
;; Or did in an old version?


;; ## Datview schema spec


;; ## Import

;; This is a great ingestion format
;; Make it possible to build semantic parsers for data on top of other web pages :-)



;; ## Attribute view

;; View all of the values for some entity, attribute pair
;; Values must be passed in explicitly, or in an atom

(defn lablify-attr-ident
  [attr-ident]
  (let [[x & xs] (clojure.string/split (name attr-ident) #"-")]
    (clojure.string/join " " (concat [(clojure.string/capitalize x)] xs))))

(defn label-view
  [app attr-ident]
  (when attr-ident
    [re-com/label
     :style {:font-size "14px"
             :font-weight "bold"}
     :label
     ;; XXX Again, should be pull-based
     (or @(posh/q (:conn app)
                  '[:find ?attr-label .
                    :in $ ?attr-ident
                    :where [?attr :db/ident ?attr-ident]
                           [?attr :attribute/label ?attr-label]]
                  attr-ident)
         (lablify-attr-ident attr-ident))]))




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

;; XXX This will be coming to posh soon, but in case we need it earlier

;; Still have to implement notion of hidden attributes at a database level


;; Still need to hook up with customized context
(defn pull-view-controls
  [app pull-expr pull-data]
  (let [pull-data (utils/deref-or-value pull-data)
        view-spec (meta pull-expr)]
    [:div (:dom/attrs @(component-context app ::pull-view-controls {::locals (meta pull-expr)}))
     [re-com/md-icon-button :md-icon-name "zmdi-copy"
                            :size :smaller
                            :style {:margin-right "10px"}
                            :tooltip "Copy entity"
                            :on-click (fn [] (js/alert "Coming soon to a database application near you"))]
     [re-com/md-icon-button :md-icon-name "zmdi-edit"
                            :style {:margin-right "10px"}
                            :size :smaller
                            :tooltip "Edit entity"
                            ;; This assumes the pull has :dat.sync.remote.db/id... automate?
                            :on-click (fn [] (router/set-route! app {:handler :edit-entity :route-params {:db/id (:dat.sync.remote.db/id pull-data)}}))]]))

(defn default-field-for-controls
  [app pull-expr pull-data]
  (let [context (component-context app ::default-field-for-controls {::locals (meta pull-expr)})]
    [:div (:dom/attrs context)])) 

;(defn)

(defn value-view
  [app pull-expr attr-ident value]
  (let [attr-sig @(attribute-signature-reaction app attr-ident)
        context @(component-context app ::value-view {::locals (meta pull-expr)})]
    [:div (:dom/attrs context)
     ;[debug "Here is the comp-attrs:" attr-sig]
     (match [attr-sig]
       ;; For now, all refs render the same; May treat component vs non-comp separately later
       [{:db/valueType :db.type/ref}]
       [pull-data-view app (get-nested-pull-expr pull-expr attr-ident) value]
       ;; Miscellaneous value
       :else
       (str value))]))

  

;; Should we have a macro for building these components and dealing with all the state in the context? Did the merge for you?
;(defn build-view-component)

(defn attr-values-view
  [app pull-expr attr-ident values]
  (let [context @(component-context app ::attr-values-view {::locals (meta pull-expr)})
        collapsable? (:dat.view.collapse/collapsable? context)
        ;; Should put all of the collapsed values in something we can serialize, so we always know what's collapsed
        collapse-attribute? (r/atom (:dat.view.collapse/default context))]
    (fn [app pull-expr attr-ident values]
      [:div (:dom/attrs context)
       (when collapsable?
         [collapse-button collapse-attribute?])
       (when @collapse-attribute?
         [collapse-summary app context values])
          ;(defn pull-summary-view [app pull-expr pull-data]
       (when (or (not collapsable?) (and collapsable? (not @collapse-attribute?)))
         (for [value (utils/deref-or-value values)]
           ^{:key (hash value)}
           [value-view app pull-expr attr-ident value]))])))



;; Need to have controls etc here
(defn attr-view
  [app pull-expr attr-ident values]
  [:div (:dom/attrs @(component-context app ::attr-view {:dat.view/locals (meta pull-expr)})) 
  ;[:div @(attribute-context app (meta pull-expr) :attr-view)
   [label-view app attr-ident]
   (match [@(attribute-signature-reaction app attr-ident)]
     [{:db/cardinality :db.cardinality/many}]
     [attr-values-view app pull-expr attr-ident values]
     :else
     [value-view app pull-expr attr-ident values])])


;; ## Security

;; All messages have signatures; or can
;; Can b e used to assert the h8istry of things

;^{:set asside}

;; You can even save your app metadata-query structures in the database :-)
;; You can build these things atomically
;; It's the perfect backbone, really
;; Subsets fine


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
                        (keys pull-data))))))
        flatten
        distinct))
  ([pull-expr]
   (pull-attributes pull-expr [])))

;; Should actually try to tackle this

(defn pull-data-view
  "Given a DS connection, a app pull-expression and data from that pull expression (possibly as a reaction),
  render the UI subject to the pull-expr metadata."
  ;; Should be able to bind the data to the type dictated by pull expr
  ([app, pull-expr, pull-data]
   ;; Annoying to have to do this
   (let [context @(component-context app ::pull-data-view {:dat.view/locals (meta pull-expr)})
         pull-data (utils/deref-or-value pull-data)]
     [:div (:dom/attrs context)
      [:div
        (when-let [controls @(component-context app ::pull-view-controls)]
          [(:dat.view/component controls) app pull-expr pull-data])
        (when-let [summary (:summary context)]
          [:div {:style (merge h-box-styles)}
           [summary app pull-expr pull-data]])]
      ;; XXX TODO Questions:
      ;; Need a react-id function that lets us repeat attrs when needed
      (for [attr-ident (pull-attributes pull-expr pull-data)]
        ^{:key (hash attr-ident)}
        [attr-view app pull-expr attr-ident (get pull-data attr-ident)])])))

(defn pull-view
  ([app pull-expr eid]
   [pull-data-view app pull-expr (posh/pull (:conn app) pull-expr eid)]))


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
     ::component attr-values-view
     ;; Right now only cardinality many attributes are collapsable; Should be able to set any? Then set for cardinality many as a default? XXX
     :dat.view.collapse/collapsable? true
     :dat.view.collapse/default false} ;; Default; nothing is collapsed
    ::value-view
    {:dom/attrs {:style (merge h-box-styles
                               {:padding "3px"})}
     ::component value-view}
    ::attr-view
    {:dom/attrs {:style (merge v-box-styles
                               {:padding "5px 12px"})}
     ::component attr-view}
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
    ::pull-view-controls
    {:dom/attrs {:style (merge h-box-styles
                               {:padding "5px"})}
                                ;:background "#DADADA"})}
                                ;;; Check if these actually make sense
                                ;:justify-content "flex-end"})}}
                                ;:gap "10px"
     ::component pull-view-controls}
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
      (let [base-schema (utils/deep-merge dat.sync/base-schema (:datascript/schema config))
            ;; Should try switching to r/atom
            ;conn (or conn (::conn config) (r/atom (d/empty-db base-schema)))
            conn (or conn (::conn config) (d/create-conn base-schema))
            main (or main (::main config))
            component (assoc component :conn conn :main main)]
        ;; Transact default settings to db
        (d/transact! conn default-settings)
        ;; Start posh
        (posh/posh! conn)
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
     :or [dat.view/base-context default-base-context]}] ;; Need to actually plug this in as an atom
   (map->Datview {:config config}))
  ([]
   (new-datview {})))


