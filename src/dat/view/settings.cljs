(ns dat.view.settings
  (:require [datascript.core :as d]
            [dat.reactor :as reactor]
            [dat.reactor.dispatcher :as dispatcher]
            [posh.core :as posh]))



;; Should really split these up...
(def schema
  ;; How to create singleton entities?
  {:db/ident {:db/unique :db.unique/identity}
   :dat.view/settings {}
   ;; All routing should be based on a routing entity; future XXX
   :dat.view.router/current-path {}})


(defn init!
  [app]
  (let [tx [{:db/id (d/tempid -1)
             :db/ident :dat.view/settings
             :dat.view.router/current-path js/window.location.pathname}]]
    (d/transact! (:conn app) tx)))


;; We should just be registering transaction functions

(reactor/register-handler ::update
  (fn [app db [_ [setting-ident setting-value]]]
    (let [tx [{:db/ident :dat.view/settings setting-ident setting-value}]]
      (reactor/resolve-to app db [[:dat.reactor/local-tx tx]]))))


;; The old implementation; TODO Remove...
;(defn update-setting
;  [conn attr-ident new-value]
;  (let [settings-id @(posh/q conn '[:find ?settings . :where [?settings :db/ident :client/settings]])]
;    (d/transact! conn [{:db/id settings-id attr-ident new-value}])))

(defn update-setting
  [app setting new-value]
  (dispatcher/dispatch! app [::update [setting new-value]]))

;; ohh... should have update-settings for bulk single transaction update as well...

(defn get-setting
  ([app setting-ident]
   (posh/q (:conn app) [:find '?x '. :where ['?settings :db/ident :dat.view/settings] ['?settings setting-ident '?x]]))
  ([app]
   (posh/pull (:conn app) '[*] [:db/ident :dat.view/settings])))

;; TODO Remove
;(defn get-settings
;  "Returns a posh reactive query for the settings entity"
;  [conn]
;  (posh/q conn '[:find (pull ?settings [*]) . :where [?settings :db/ident :client/settings]]))

;; TODO Buid out stuff for syncing settings objects for users




