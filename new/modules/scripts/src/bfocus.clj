(ns bfocus
  (:require
   [cats.monad.either :as either]
   [cats.core :as m]
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
  (assoc state :time/current {:start (or start (t/now))
                              :duration (or duration (:default-duration config))}))

(defn remaining-timer-duration [state]
  (when-let [{:keys [start duration]} (:time/current state)]
    (t/between (t/now)
               (lib.time/+ start duration))))

(defn current-timer [state]
  (or (some-> (remaining-timer-duration state)
              (either/right))
      (either/left :error/no-timer)))

;; Routes ----------------------------------------------------------------------

(defn start-timer! [opts]
  (->> (set-current-timer opts @!state)
       (reset! !state)))

(comment
  (start-timer! {})
  (m/mlet [duration (current-timer @!state)]
    (m/return (lib.time/format-duration duration)))
  nil)


;; Server ----------------------------------------------------------------------

(def routes
  [{:path     "/"
    :method   :get
    :response ()}])

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
         (reset! srv))))
