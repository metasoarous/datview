(ns dat.view.settings
  (:require [datascript.core :as d]
            [reagent.core :as r]
            [reagent.ratom :refer-macros [reaction]]
            [dat.reactor :as reactor]
            [dat.reactor.dispatcher :as dispatcher]
            [posh.reagent :as posh]))



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
;  (let [settings-id @(posh/q '[:find ?settings . :where [?settings :db/ident :client/settings]] conn)]
;    (d/transact! conn [{:db/id settings-id attr-ident new-value}])))

(defn update-setting
  [app setting new-value]
  (dispatcher/dispatch! app [::update [setting new-value]]))

;; ohh... should have update-settings for bulk single transaction update as well...

(def safe-pull
  (memoize
    (fn safe-pull*
      ([conn pull-expr id default]
       (reaction
        (try
          @(posh/pull conn pull-expr default)
          (catch :default e
            default))))
      ([conn pull-expr id]
       (safe-pull* conn pull-expr id nil)))))

(defn get-setting
  ([app setting-ident]
   (posh/q [:find '?x '. :where ['?settings :db/ident :dat.view/settings] ['?settings setting-ident '?x]] (:conn app)))
  ([app]
   (safe-pull (:conn app) '[*] [:db/ident :dat.view/settings])))

;; TODO Remove
;(defn get-settings
;  "Returns a posh reactive query for the settings entity"
;  [conn]
;  (posh/q '[:find (pull ?settings [*]) . :where [?settings :db/ident :client/settings]] (:conn app)))

;; TODO Buid out stuff for syncing settings objects for users




