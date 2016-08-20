(ns dat.sub
  (:require-macros [reagent.ratom :refer [reaction]]
                   [cljs.core.async.macros :as async-macros :refer [go go-loop]]
                   [servant.macros :refer [defservantfn]])
  (:require
    ;[dat.reactor :as reactor]
    ;[dat.reactor.dispatcher :as dispatcher]
    ;[dat.view.representation :as representation]
    ;[dat.view.router :as router]
    ;[dat.view.utils :as utils]
    ;[dat.view.context :as context]
    [dat.view.utils :as utils]
    [dat.view.query :as query]
    ;[dat.view.routes :as routes]
    ;[dat.view.settings :as settings]
    ;[dat.spec.protocols :as protocols]
    ;; Things outside datsys, but intimately tied to datsys
    [datascript.core :as d]
    [posh.reagent :as posh]
    [reagent.core :as r]
    [reagent.ratom :as ratom :include-macros true]
    [re-com.core :as re-com]
    ;; Other stuff
    [taoensso.timbre :as log :include-macros true]
    [com.stuartsierra.component :as component]
    ;[clojure.walk :as walk]
    [cljs.core.async :as async :refer [chan close! timeout put! <! >!]]
    ;[cljs.spec :as s]
    ;[cljs.core.match :as match :refer-macros [match]]
    [servant.core :as servant]
    [servant.worker :as worker]
    [com.stuartsierra.component :as component]
    [datascript.core :as d]
    [reagent.core :as r]))


;; This should realy be moved out to it's own thing, but I'll leave it in datview for now, while incubating.

(defrecord ServantManager
  [worker-count worker-script servant-channel]
  component/Lifecycle
  (start [component]
    (log/info ">> Starting ServantManager")
    (let [servant-channel (or servant-channel (servant/spawn-servants worker-count worker-script))]
      (assoc component :servant-channel servant-channel)))
  (stop [component]
    ;; Here you have the ability to specify how many servants to kill off.
    (log/info "<< Stopping ServantManager")
    (servant/kill-servants servant-channel worker-count)
    (assoc component :servant-channel nil)))


;(defn servant-reaction
;  [app cache servant-f args options]
;  (let [;conn (:conn app) ;; need to think about how to treat conn separately
;        servant-channel (-> app :servant :servant-channel)
;        options (merge default-pull-options
;                       options)
;        answer-atom (if (:dat.sub/cache? options)
;                      (if-let [cached-ans (get @cache args)]
;                        cached-ans
;                        (let [new-ans (r/atom (:dat.sub/default options))]
;                          (swap! cache assoc args new-ans)
;                          new-ans))
;                      (r/atom (:dat.sub/default options)))]
;    (reaction
;      (let [;db @conn ;; TODO This is where we need to add posh filters when appropriate
;            args (map utils/deref-or-value args)
;            result-channel (apply servant/servant-thread servant-channel servant/standard-message servant-f args)]
;        (go
;          (let [ans (<! result-channel)]
;            (reset! answer-atom ans))))
;      @answer-atom)))

(def default-servant-options
  {:worker-count 4
   ;; I think this is right...
   :worker-script "js/servant.js"})

(defn new-servant-manager
  ([options]
   (map->ServantManager (merge
                          default-servant-options
                          options)))
  ([]
   (new-servant-manager {})))


(defservantfn dostuff
  [x y]
  (* x y))


(defn dostuff-reaction
  [servant-manager x y]
  (let [a (r/atom nil)]
    (log/debug "Making a dostuff-reaction with x y:" x y)
    (log/debug "Servant channel" (:servant-channel servant-manager))
    (let [result-chan (servant/servant-thread (:servant-channel servant-manager) servant/standard-message dostuff x y)]
      (log/debug "Servant thread created!")
      (go (let [result (<! result-chan)]
            (log/debug "New dostruff-reaction result:" result)
            (reset! a result))))
    a))



(defn remote-subscription
  [remote-event]
  (let []))




