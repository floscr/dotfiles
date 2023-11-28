(ns bfocus
  (:require
   [cats.monad.either :as either]
   [cats.core :as m]
   [babashka.http-client :as http]
   [lib.time]
   [org.httpkit.server :as server]
   [tick.core :as t]))

(require '[babashka.deps :as deps])
(deps/add-deps '{:deps {org.clojars.askonomm/ruuter {:mvn/version "1.3.2"}}})
(require '[ruuter.core :as ruuter])

;; Config ----------------------------------------------------------------------

(def config {:port 42069
             :default-duration (t/new-duration 1 :hours)})

;; State -----------------------------------------------------------------------

(defonce !state (atom {:config config}))

(defn set-current-timer [{:keys [start duration]} state]
  (assoc state :time/current {:status :timer/active
                              :start (or start (t/now))
                              :duration (or duration (:default-duration config))}))

(defn remove-current-timer [state]
  (dissoc state :time/current))

(defn remaining-timer-duration [state]
  (when-let [{:keys [start duration]} (:time/current state)]
    (t/between (t/now) (lib.time/+ start duration))))

(defn current-timer! []
  (remaining-timer-duration @!state))

(defn start-timer! [opts]
  (->> (set-current-timer opts @!state)
       (reset! !state)))

(defn stop-timer! [opts]
  (->> (remove-current-timer @!state)
       (reset! !state)))

;; Routes ----------------------------------------------------------------------

(defn main-route [opts]
  {:status 200
   :body "Hello"})

(defn start-route [opts]
  (start-timer! opts)
  {:status 200})

(defn stop-route [opts]
  (stop-timer! opts)
  {:status 200})

(defn print-current-timer-route [opts]
  (if-let [duration (current-timer!)]
    {:status 200
     :body (lib.time/format-duration duration)}
    {:status 500}))

(comment
  (start-timer! {})
  (stop-timer! {})
  (current-timer!)
  (print-current-timer-route {})
  nil)

;; Server ----------------------------------------------------------------------

(def routes
  [{:path     "/"
    :method   :get
    :response main-route}
   {:path "/start"
    :method :get
    :response start-route}
   {:path "/stop"
    :method :get
    :response stop-route}
   {:path "/print-current-timer"
    :method :get
    :response print-current-timer-route}])

(defn server-start! []
  (server/run-server #(ruuter/route routes %) {:port (:port config)}))

(defn server-stop! [srv]
  (srv))

;; Server ----------------------------------------------------------------------

(defn main []
  (server-start!)
  (println "serving" (:server-url config))
  @(promise))

(when (= *file* (System/getProperty "babashka.file"))
  (main))

;; Repl ------------------------------------------------------------------------

(comment
  (do
    (defonce srv (atom nil))
    (some-> @srv server-stop!)
    ;; Always print to repl
    (alter-var-root #'*out* (constantly *out*))
    (->> (server/run-server #(ruuter/route routes %) {:port (:port config)
                                                      :event-logger println
                                                      :warn-logger println
                                                      :error-logger println})
         (reset! srv)))

  (http/get "http://localhost:42069/start")
  (http/get "http://localhost:42069/stop")
  (http/get "http://localhost:42069/print-current-timer"))
