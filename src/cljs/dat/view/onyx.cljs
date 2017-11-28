(ns dat.view.onyx
  (:require [taoensso.timbre :as log]
            [onyx.sim.api :as onyx]
            [onyx.sim.flui :as flui]
            [datascript.core :as d]
            [posh.reagent :as posh]
            [datascript.parser :as dparse]
            [datascript.db :as ddb]
            [onyx.sim.core :as sim]
            [onyx.sim.event :as event]))

;;;
;;; Predicates
;;;
(defn ^:export represent? [event old-seg seg all-new represent]
;;   (log/info "represent?" represent (= (:dat.view.rep/handler seg) represent))
  (= (:dat.view.rep/handler seg) represent))

;;;
;;; Helpers
;;;
(defn ^:export map-alias [alias-map seg]
  (into
    seg
    (map
      (fn [[alias-attr path]]
        [alias-attr (get-in seg path)]))
    alias-map))

(defn query->map [q]
  (cond
    (map? q) q
    (sequential? q) (dparse/query->map q)
    :else (ddb/raise "Query should be a vector or a map"
                 {:error :parser/query, :form q})))

(defn parse-find-vars [q-expr]
  ;; TODO: implement. for now a hardcoded value for testing
  (log/info "dparse-expr" q-expr)
  (let [find-vars (dparse/collect-vars-distinct (dparse/parse-find (:find (query->map q-expr))))
        find-keys (mapv
                    (fn [var]
                      (keyword (:symbol var)))
                    find-vars)]
    find-keys))

(defn process-event [db env {:as seg :keys [onyx.sim/sim]}]
    (-> (onyx/new-segment env :dat.view/dispatch (assoc seg :dat.sync.db/snapshot db))
        onyx/drain
        :tasks
        :dat.sync/transact
        :outputs
        first
        :dat.sync.db/txs))

(defn dispatch! [{:as sys :keys [dat.sync.db/conn onyx.sim/sim]}
                 {:as seg :keys [dat.view.event/context]}
                 & inputs]
  ;; ???: what happens when you listen to a ratom inside a dispatch method?
  ;; ???: make dispatch! a lifecycle item.
  (d/transact!
    conn
    [[:db.fn/call
      process-event
      @(event/subscribe-clean-env conn sim)
      (assoc
        (select-keys seg (into #{:dat.view.event/handler :dat.view.event/entity :dat.view.event/attr} context))
        :dat.view.event/inputs
        inputs)]]))

;;;
;;; Event
;;;
(defn ^:export simple-toggle [db {:keys [dat.view.event/entity dat.view.event/attr]}]
  (let [{:as e :keys [db/id]} (d/entity db entity)
        v (get e attr)]
;;     (log/info "toggle" [id attr] "from" v "to" (not v))
    [[:db/retract id attr v]
     [:db/add id attr (not v)]]))

(defn ^:export simple-value [db {:keys [dat.view.event/entity dat.view.event/attr dat.view.event/inputs]}]
  (let [{:as e :keys [db/id]} (d/entity db entity)
        old-v (get e attr)
        new-v (first inputs)]
;;     (log/info "change" [id attr] "from" old-v "to" new-v)
    [[:db/retract id attr old-v]
     [:db/add id attr new-v]]))

(defn ^:export intent [{:as seg :keys [dat.sync.db/snapshot dat.view.event/handler]}]
;;   (log/info "intenting" handler seg)
  {:dat.sync.db/txs
   (case handler
     ::simple-toggle (simple-toggle snapshot seg)
     ::simple-value (simple-value snapshot seg)
     (throw (ex-info "Unknown intent" {:segment seg})))})

;;;
;;; Lifecycle
;;;
(defn inject-meta [event lifecycle]
  {:onyx.core/params
   (conj (:onyx.core/params event) (meta lifecycle))})

(def ^:export meta-lifecycle
  {:lifecycle/before-task-start inject-meta})

(defn ^:export sim-render [outputs]
  [flui/h-box
   :children
   (mapv :dat.view.rep/component outputs)])

;;;
;;; Onyx render
;;;
(defn render-segment [{:as sys :keys [dat.sync.db/conn onyx.sim/sim]}
                      {:as seg}]
;;   (log/info "rendering seg" seg)
  (let [env @(event/subscribe-clean-env conn sim)]
    ;; !!!: (log/info "clean" (onyx/env-summary env))
    (try
      (->
        (onyx/new-segment env :dat.view/render seg)
        onyx/drain
        :tasks
        :dat.view/mount
        :outputs
        first
        :dat.view.rep/component)
      (catch :default e
        [sim/sim-debug
         sys
         {:onyx.sim/inputs {:dat.view/render [seg]}
          :onyx.sim/error e}]))))


;; (defn render-segments->debug-sim [db parent-sim-id segs child-name]
;;   ;; TODO: update to use make-sim so all the init is done properly.
;;   (let [parent-sim (d/entity db parent-sim-id)]
;;     (log/info "parent job" (:onyx.core/job parent-sim))
;;     nil
;; ;;   [(into
;; ;;      sim/default-sim
;; ;;      {:onyx.sim/title child-name
;; ;;       :onyx/type :onyx.sim/sim
;; ;;       :onyx.core/job (get-in parent-sim [:onyx.core/job :db/id])
;; ;;       :onyx.sim/env (reduce #(onyx/new-segment %1 :dat.view/render %2) (:onyx.sim/clean-env parent-sim) segs)
;; ;;       :onyx.sim/clean-env (:onyx.sim/clean-env parent-sim)})]
;;     ))

(defn box* [{:as sys :keys [dat.sync.db/conn onyx.sim/sim]}
            {:as seg :keys [dat.view.rep/direction
                            dat.view.rep/layout
                            dat.view.rep/style]}]
  (let [children (map
                   (fn [child]
                     [render-segment
                      sys
                      child])
                   layout)]
;;     (log/info "dat-view-box")
    [flui/v-box
     :children
     [
;;       [flui/button
;;        :label "Convert to Simulator"
;;        :on-click #(d/transact! conn [[:db.fn/call render-segments->debug-sim sim layout "Spawned Sim"]])]
      [(case direction
         :horizontal flui/h-box
         flui/v-box)
       :style style
       :children (vec children)]]]))

;;;
;;; View Component Tasks
;;;
(defn ^:export box [sys seg]
  (assoc
    seg
    :dat.view.rep/component
    [box* sys seg]))

(defn ^:export label [{:as seg :keys [dat.view.rep/label]}]
  (assoc
    seg
    :dat.view.rep/component
    [flui/label :label label]))

(defn ^:export checkbox [sys
                         {:as seg :keys [dat.view.rep/label
                                         dat.view.rep/toggled?]}]
  (let [default-event {:dat.view.event/handler ::simple-toggle
                       :dat.view.event/entity (:db/id seg)
                       :dat.view.event/attr :dat.view.rep/toggled?}]
    (assoc
      seg
      :dat.view.rep/component
      [flui/checkbox
       :model toggled?
       :label label
       :on-change #(dispatch! sys (into default-event seg))])))

(defn ^:export text-input [sys
                           {:as seg :keys [dat.view.rep/label]}]
;;   (log/info "text-input" label (:onyx.sim/sim sys) event)
  (assoc
    seg
    :dat.view.rep/component
    [flui/input-text
     :model label
     :on-change (partial dispatch! sys seg)]))

(defn ^:export default [seg]
  (assoc
    seg
    :dat.view.rep/component
    [flui/p (str "Unknown representation:" seg)]))



;;;
;;; Subscription Tasks
;;;
(defn ^:export route [{:as sys :keys [dat.sync.db/conn]}
                      {:as seg :keys [dat.view/route
                                      db/id]}]
;;   (log/info "Routing (or " id route)
;;   (log/info "  conn" conn)
;;   (log/info "  pull" (d/pull @conn '[*] (or id [:dat.view/route route])) ")")
  (into
    seg
    @(posh/pull conn '[*] (or id [:dat.view/route route]))))

(defn ^:export pull [{:as sys :keys [dat.sync.db/conn]}
                     {:as seg :keys [dat.view.sub/pull-expr
                                     dat.view.sub/handler
                                     dat.view.sub/entity
                                     dat.view.sub/alias]}]
;;   (log/info "pull")
;;   (log/info pull-expr entity)
  (map-alias
    alias
    (into
      seg
      (when (= handler :dat.view.subscribe/pull)
        @(posh/pull conn pull-expr entity)))))

(defn ^:export query [{:as sys :keys [dat.sync.db/conn]}
                      {:as seg :keys [dat.view.sub/q-expr
                                      dat.view.sub/inputs
                                      dat.view.sub/handler
                                      dat.view.sub/alias
                                      dat.view.sub/row-alias
                                      dat.view.sub/row-base]}]
;;   (log/info "dat-view-query")
  (if-not (= handler :dat.view.subscribe/query)
    seg
    (let [find-vars (parse-find-vars q-expr)
          relation @(apply posh/q q-expr conn inputs)]
      (map-alias
        alias
        (assoc
          seg
          :dat.view.sub/relation
          (mapv
            (fn [row]
              (into
                (or row-base {})
                (map-alias row-alias (zipmap find-vars row))))
            relation))))))


;;;
;;; Render Job
;;;
(defn catalog
  ([] (catalog 20))
  ([onyx-batch-size]
   [{:onyx/name :dat.view/render
     :onyx/doc "Segments to be rendered and eventually mounted"
     :onyx/type :input
     :onyx/batch-size onyx-batch-size}
    {:onyx/name :dat.view/dispatch
     :onyx/doc "Segments that are events to be dispatched and eventually transacted"
     :onyx/type :input
     :onyx/batch-size onyx-batch-size}

    {:onyx/name :dat.view.event/intent
     :onyx/doc "Matches events to handlers."
     :onyx/type :function
     :onyx/fn ::intent
     :onyx/batch-size onyx-batch-size}

    {:onyx/name :dat.view.sub/route
     ;; TODO: can we get the doc from the funtion itself???
     ;; ???: markdown
     :onyx/doc "This posh/pulls '[*] the :dat.view/route and merges into the segment"
     :onyx/type :function
     :onyx/fn ::route
     :onyx/batch-size onyx-batch-size}
    {:onyx/name :dat.view.subscribe/pull
     :onyx/doc "If this segment has a :dat.view.sub/pull-expr, then posh/pull"
     :onyx/type :function
     :onyx/fn ::pull
     :onyx/batch-size onyx-batch-size}
    {:onyx/name :dat.view.subscribe/query
     :onyx/doc "If this segment has a :dat.view/query-expr, then posh/q"
     :onyx/type :function
     :onyx/fn ::query
     :onyx/batch-size onyx-batch-size}

    {:onyx/name :dat.view.represent/box
     :onyx/type :function
     :onyx/fn ::box
     :onyx/batch-size onyx-batch-size}
    {:onyx/name :dat.view.represent/label
     :onyx/type :function
     :onyx/fn ::label
     :onyx/batch-size onyx-batch-size}
    {:onyx/name :dat.view.represent/checkbox
     :onyx/type :function
     :onyx/fn ::checkbox
     :onyx/batch-size onyx-batch-size}
    {:onyx/name :dat.view.represent/text-input
     :onyx/type :function
     :onyx/fn ::text-input
     :onyx/batch-size onyx-batch-size}
    {:onyx/name :dat.view.represent/default
     :onyx/type :function
     :onyx/fn ::default
     :onyx/batch-size onyx-batch-size}

    {:onyx/name :dat.view/mount
     :onyx/type :output
     :onyx.sim/render sim-render
     :onyt/batch-size onyx-batch-size}
    {:onyx/name :dat.sync/transact
     :onyx/type :output
     :onyx/batch-size onyx-batch-size}]))

(def job
  {:onyx/type :onyx.core/job
   :onyx.core/catalog (catalog)

   :onyx.core/lifecycles
   [{:lifecycle/task :dat.view.sub/route
     :lifecycle/calls ::meta-lifecycle}
    {:lifecycle/task :dat.view.subscribe/pull
     :lifecycle/calls ::meta-lifecycle}
    {:lifecycle/task :dat.view.subscribe/query
     :lifecycle/calls ::meta-lifecycle}
    {:lifecycle/task :dat.view.represent/box
     :lifecycle/calls ::meta-lifecycle}
    {:lifecycle/task :dat.view.represent/checkbox
     :lifecycle/calls ::meta-lifecycle}
    {:lifecycle/task :dat.view.represent/text-input
     :lifecycle/calls ::meta-lifecycle}]

   :onyx.core/workflow
   [[:dat.view/dispatch :dat.view.event/intent]
    [:dat.view.event/intent :dat.sync/transact]
    [:dat.view/render :dat.view.sub/route]
    [:dat.view.sub/route :dat.view.subscribe/query]
    [:dat.view.subscribe/query :dat.view.subscribe/pull]

    [:dat.view.subscribe/pull :dat.view.represent/default]
    [:dat.view.subscribe/pull :dat.view.represent/label]
    [:dat.view.subscribe/pull :dat.view.represent/checkbox]
    [:dat.view.subscribe/pull :dat.view.represent/text-input]
    [:dat.view.subscribe/pull :dat.view.represent/box]

    [:dat.view.represent/default :dat.view/mount]
    [:dat.view.represent/label :dat.view/mount]
    [:dat.view.represent/checkbox :dat.view/mount]
    [:dat.view.represent/text-input :dat.view/mount]
    [:dat.view.represent/box :dat.view/mount]]

   :onyx.core/flow-conditions
   [{:flow/from :dat.view.subscribe/pull
     :flow/to [:dat.view.represent/box]
     :dat.view.rep/handler :dat.view.represent/box
     :flow/predicate [::represent? :dat.view.rep/handler]
     :flow/short-circuit? true}
    {:flow/from :dat.view.subscribe/pull
     :flow/to [:dat.view.represent/label]
     :dat.view.rep/handler :dat.view.represent/label
     :flow/predicate [::represent? :dat.view.rep/handler]
     :flow/short-circuit? true}
    {:flow/from :dat.view.subscribe/pull
     :flow/to [:dat.view.represent/checkbox]
     :dat.view.rep/handler :dat.view.represent/checkbox
     :flow/predicate [::represent? :dat.view.rep/handler]
     :flow/short-circuit? true}
    {:flow/from :dat.view.subscribe/pull
     :flow/to [:dat.view.represent/text-input]
     :dat.view.rep/handler :dat.view.represent/text-input
     :flow/predicate [::represent? :dat.view.rep/handler]
     :flow/short-circuit? true}
    {:flow/from :dat.view.subscribe/pull
     :flow/to [:dat.view.represent/default]
     :flow/predicate :onyx.sim.core/always
     :flow/short-circuit? true}]})

(defn make-sim
  ([conn] (make-sim conn :dat.view/sim))
  ([conn sim-name]
   (sim/make-sim
     :name sim-name
     :title "Dat View Simulator"
     :description "This will simulate the compute graph for dat view."
     :job (update-in
            job
            [:onyx.core/lifecycles]
            (fn [lifecycles]
              (for [lc lifecycles]
                (with-meta lc {:dat.sync.db/conn conn
                               :onyx.sim/sim [:onyx/name sim-name]}))))
     :transitions [{:event :onyx.sim.api/inputs
                    :inputs {:dat.view/render
                             [{:dat.view/route :dat.view.route/todos}
                              {:dat.view/route :dat.view.route/index}]}}])))

(def todos-query
  '[:find ?todo
    :in $
    :where
    [?todo :e/type ?todo-type]
    [?todo-type :db/ident :e.type/Todo]])

(def examples
   [
;;      {:e/type :e.type/Todo
;;      :e/name "Test Todo"
;;      :dat.view.rep/toggled? false}
;;     {:onyx/name :example-label
;;      :e/type :e.type/Todo
;;      :e/name "Bake a Cake"
;;      :dat.view.rep/toggled? true}

    {:dat.view/route :dat.view/hello-world
     :dat.view.rep/handler :dat.view.represent/label
     :dat.view.rep/label "Hello, World!"}
    {:dat.view/route :dat.view/bye-world
     :dat.view.rep/handler :dat.view.represent/text-input
     :dat.view.event/handler ::simple-value
     :dat.view.event/entity [:dat.view/route :dat.view/bye-world]
     :dat.view.event/attr :dat.view.rep/label
     :dat.view.rep/label "Goodbye, World!"}
    {:dat.view/route :dat.view/bye-world2
     :dat.view.sub/handler :dat.view.subscribe/pull
     :dat.view.rep/handler :dat.view.represent/label
     :dat.view.sub/pull-expr [:dat.view.rep/label]
     :dat.view.sub/entity [:dat.view/route :dat.view/bye-world]}

    {:dat.view/route :example/todo-name
     :dat.view.sub/handler :dat.view.subscribe/pull
     :dat.view.rep/handler :dat.view.represent/label
     :dat.view.sub/pull-expr [:e/name]
     :dat.view.sub/alias {:dat.view.rep/label [:e/name]}
     :dat.view.sub/entity [:onyx/name :example-label]}

    {:dat.view/route :dat.view.route/index
     :dat.view.rep/style {:background-color :LightGray}
     :dat.view.rep/direction :vertical
     :dat.view.rep/handler :dat.view.represent/box
     :dat.view.rep/layout [[:dat.view/route :dat.view/hello-world]
                           [:dat.view/route :dat.view/bye-world]
                           [:dat.view/route :dat.view/bye-world2]]}

    {:dat.view/route :dat.view.route/todos
     :dat.view.sub/handler :dat.view.subscribe/query
     :dat.view.rep/handler :dat.view.represent/box
     :dat.view.sub/q-expr todos-query
     :dat.view.sub/row-base {:dat.view/route :dat.view.route/todo}
     :dat.view.sub/row-alias '{:dat.view.sub/entity [:?todo]}
     :dat.view.sub/alias {:dat.view.rep/layout [:dat.view.sub/relation]}}
    {:dat.view/route :dat.view.route/todo
     :dat.view.sub/handler :dat.view.subscribe/pull
     :dat.view.rep/handler :dat.view.represent/checkbox
     :dat.view.sub/pull-expr `[:e/name :dat.view.rep/toggled?]
     :dat.view.sub/alias {:dat.view.rep/label [:e/name]
                          :dat.view.event/entity [:db/id]}
     ;; Note: these event attrss are optional since they are identical to the default event for checkbox
     :dat.view.event/handler ::simple-toggle
     :dat.view.event/attr :dat.view.rep/toggled?}])
