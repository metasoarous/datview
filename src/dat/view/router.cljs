(ns dat.view.router
  (:require [bidi.bidi :as bidi]
            [dat.view.settings :as settings]
            [dat.view.routes :as routes]
            [datascript.core :as d]
            [goog.events])
  (:import [goog.history Html5History EventType]))


;(defprotocol Router
  ;(route))


;; Now we define how to instatiate the history object.

(defn make-history []
  (doto (Html5History.)
    (.setPathPrefix (str js/window.location.protocol
                         "//"
                         js/window.location.host))
    (.setUseFragment false)))


;; Now we set up our global history object. We use defonce so we can hot reload the code.

;; We should maybe be moving this into a constructor or something so that this state can be in the main app ns

(defn attach-history-handler!
  [history handler-fn]
  (doto history
    (goog.events/listen EventType.NAVIGATE
                        ;; wrap in a fn to allow live reloading
                        #(handler-fn %))
    (.setEnabled true)))


(defn update-route!
  [app]
  ;; If we put this in here, for the API we have to somenow let you add your own route customizations... XXX
  (settings/update-setting app :datview/route (bidi/match-route routes/routes js/window.location.pathname)))


(defn make-handler-fn
  [app]
  (fn [e]
    (update-route! app)))

;; Should rewrite these from app
(defn get-route
  [app]
  ;; Actually... :datsync/route should maybe just be it's own ident...
  (:datview/route @(settings/get-settings app)))


(defn set-route!
  [app {:as route :keys [handler route-params]}]
  (let [flattened-params (-> route-params seq flatten)
        path-for-route (apply bidi/path-for routes/routes handler flattened-params)]
    (if-not path-for-route
      (do
        (println "params:" (with-out-str (pr-str flattened-params)))
        (println "path:" path-for-route)
        (js/alert "Hit a bad route: " (pr-str route-params)))
      (.setToken (:datview/history-obj @(settings/get-settings app))
                 path-for-route))))

