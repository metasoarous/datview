(ns dat.view.forms
  "# Datview forms"
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [dat.view.router :as router]
            [dat.view.query :as query]
            ;; Need to switch to datview XXX
            [dat.view :as datview]
            [dat.view.utils :as utils]
            [datascript.core :as d]
            [posh.core :as posh]
            [reagent.core :as r]
            [re-com.core :as re-com]
            [goog.date.Date]
            [cljs-time.core :as cljs-time]
            [cljs-time.format]
            [cljs-time.coerce]
            [cljs.pprint :as pp]
            [cljs.core.match :as match :refer-macros [match]]))



(declare pull-form)


(defn cast-value-type
  [value-type-ident str-value]
  (case value-type-ident
    (:db.type/double :db.type/float) (js/parseFloat str-value)
    (:db.type/long :db.type/integer) (js/parseInt str-value)
    str-value))


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
          (datview/send-tx!
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
     (datview/send-tx! app
                       (concat [[:db/add eid attr-ident new-value]]
                               (when old-value
                                 [[:db/retract eid attr-ident old-value]]))))))


(defn select-entity-input
  {:todo ["Finish..."
          "Create some attribute indicating what entity types are possible values; other rules?"]}
  ([app eid attr-ident value]
   ;; XXX value arg should be safe as a reaction here
   (let [;options @(datview/attribute-signature-reaction app attr-ident)]
         options (->>
                   @(posh/q (:conn app)
                            '[:find [(pull ?eid [*]) ...]
                              :in $ ?attr
                              :where [?attr :attribute.ref/types ?type]
                                     [?eid :e/type ?type]]
                            [:db/ident attr-ident])
                   ;; XXX Oh... should we call entity-name entity-label? Since we're really using as the label
                   ;; here?
                   (mapv (fn [pull-data] (assoc pull-data :label (datview/pull-summary pull-data)
                                                          :id (:db/id pull-data))))
                   (sort-by :label))]
     ;; Remove this div; just for debug XXX
     [:div
      ;[datview/debug "options:" options]
      [select-entity-input app eid attr-ident value options]]))
  ([app eid attr-ident value options]
   [re-com/single-dropdown
    :style {:min-width "150px"}
    :choices options
    :model (:db/id value)
    :on-change (partial apply-reference-change! app eid attr-ident (:db/id value))]))


;; Simple md (markdown) component; Not sure if we really need to include this in datview or not...
;(defn md
  ;[md-string]
  ;[re-com/v-box
   ;:children
   ;[[:div {:dangerouslySetInnerHTML {:__html (md/md->html md-string)}}]]])


;; ### Datetimes...

;; XXX Need to get proper dat+time handlers

;(defn update-date
  ;[old-instant new-date]
  ;;; For now...
  ;(let [old-instant (cljs-time.coerce/from-date old-instant)
        ;day-time (cljs-time/minus old-instant
                                  ;(cljs-time/at-midnight old-instant))
        ;new-time (cljs-time/plus new-date
                                 ;day-time)]
    ;new-time
    ;))

(defn update-date
  [old-instant new-date]
  ;; For now...
  new-date)

(defn datetime-date-change-handler
  [app eid attr-ident current-value new-date-value]
  (let [old-value @current-value
        new-value (update-date old-value new-date-value)]
    (reset! current-value new-value)
    (datview/send-tx! app
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
                                      (datview/send-tx! app
                                                        (concat
                                                          (when-not (nil? old-value)
                                                            [[:db/retract eid attr-ident old-value]])
                                                          [[:db/add eid attr-ident new-value]]))))])))


;; XXX Having to do a bunch of work it seems to make sure that the e.type/attributes properties are set up for views to render properly;
;; We're not getting time entries showing up on ui;
;; Not sure if not making the circuit or if something weird is going on.

;; XXX Also, it seems like right now we need the :db/id in the pull expressions; Need to find a way of requesting for other data when needed

;; XXX Should have option for collapse that would let you collapse all instances of some attribute, versus just one particular entity/attribute combo

;; Should have this effectively mutlitmethod dispatch using the datview customization functionality
(defn input-for
  ([app context pull-expr eid attr-ident value]
   ;; XXX TODO Need to base this on the generalized stuff
   (let [attr @(datview/attribute-signature-reaction app attr-ident)]
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
  (datview/send-tx!
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
     [[datview/label-view app attr-ident]
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
  (let [attr-sig (datview/attribute-signature-reaction app attr-ident)
        config (datview/component-context app ::field-for {:dat.view/locals context :dat.view/attr attr-ident})
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
           ;[datview/debug "type-idents:" type-idents]
           ;[datview/debug "attr-sig:" @attr-sig]
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
                ;[datview/debug "value:" value]
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
        (datview/send-tx! app [[:db.fn/retractEntity eid]])))))


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
       (let [context @(datview/component-context app ::pull-form {:dat.view/locals context})]
         [:div (:dom/attrs context)
          ;; Can you doubly nest for loops like this? XXX WARN
          (for [attr-ident (pull-expr-attributes app pull-expr)]
            ^{:key (hash attr-ident)}
            [field-for app context pull-expr (:db/id pull-data-or-eid) attr-ident (get pull-data-or-eid attr-ident)])])))))

;; We should use this to grab the pull expression for a given chunk of data
;(defn pull-expr-for-data

;(defn edit-entity-form
  ;[app remote-eid]
  ;(if-let [eid @(posh/q (:conn app) '[:find ?e . :in $ ?remote-eid :where [?e :datsync.remote.db/id ?remote-eid]] remote-eid)]
    ;[re-com/v-box :children [[pull-data-form app eid]]]
    ;[loading-notification "Please wait; form data is loading."]))


;; These are our new goals

;(defn pull-data-form
  ;[app pull-expr eid]
  ;(if-let [current-data @(posh/pull (:conn app) pull-expr eid)]
    ;[re-com/v-box :children [[edit-entity-fieldset app eid]]]
    ;[loading-notification "Please wait; loading data."]))

;(defn pull-form
  ;[app pull-expr eid])


(swap! datview/default-base-context
       utils/deep-merge
  ;; Top level just says that this is our configuration? Or is that not necessary?
  {:dat.view/base-config
   {::pull-form
    {:dom/attrs {:style datview/bordered-box-style}}}
   ;; Specifications merged in for any config
   :dat.view/card-config {}
   ;; Specifications merged in for any value type
   :dat.view/value-type-config {}
   :dat.view/attr-config {}})


