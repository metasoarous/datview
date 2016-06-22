(ns dat.view.settings
  (:require [datascript.core :as d]
            [posh.core :as posh]))
            


;; Should really split these up...
(def schema
  ;; How to create singleton entities?
  {:db/ident {:db/unique :db.unique/identity}
   :client/settings {:db/valueType :db.type/ref
                     :db/unique :db.unique/identity
                     :db/ident :client/settings}
   ;; All routing should be based on a routing entity; future XXX
   :datview.route/entity {:db/valueType :db.type/ref}
   ;; XXX Not sure how much of this stuff should be installed here, vs in separate more modular entities
   ;:datsync.scope/query {:db/unique :db.unique/value}
   ;:datsync.scope/page {:db/unique :db.unique/value}
   ;:datsync.scope/per-page {:db/unique :db.unique/value}
   ;:datsync.scope/level {:db/unique :db.unique/value}
   ;;; Should be the attr-ident of something at the level of level, and point to something sortable
   ;:datsync.scope/order-by {:db/unique :db.unique/value}
   ;;; Should be :datsync.scope.order/desc or :datsync.scope.order/asc
   ;:datsync.scope/order-dir {:db/unique :db.unique/value}
   ;; Should be either :datview.mode/entity or :datview.mode/table
   ;:datview/mode {:db/unique :db.unique/value}
   :datview/path {:db/unique :db.unique/value}})


(defn init-client-settings!
  [conn]
  ;; I don't think it's actually using this yet...
  (d/transact! conn [{:db/id -1
                      :db/ident :client/settings
                      :datsync.scope/query ""
                      :datsync.scope/page 0
                      :datsync.scope/per-page 10
                      :datsync.scope/order-by :e/order
                      :datview/path js/window.location.pathname}]))
                      

(defn update-setting
  [conn attr-ident new-value]
  (let [settings-id @(posh/q conn '[:find ?settings . :where [?settings :db/ident :client/settings]])]
    (d/transact! conn [{:db/id settings-id attr-ident new-value}])))

;; ohh... should have update-settings for bulk single transaction update as well...

(defn get-settings
  "Returns a posh reactive query for the settings entity"
  [conn]
  (posh/q conn '[:find (pull ?settings [*]) . :where [?settings :db/ident :client/settings]]))

;; XXX Should just add a get setting helper as well...



