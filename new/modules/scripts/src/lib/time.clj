(ns lib.time
  (:require
   [tick.core :as t])
  (:refer-clojure :exclude [+ -]))

(defn unix-timestamp []
  (int (/ (.getTime (java.util.Date.)) 1000)))

(defn format-duration [duration]
  (let [seconds (t/seconds duration)
        hours (/ seconds 3600)
        minutes (/ (mod seconds 3600) 60)
        seconds (mod seconds 60)]
    (format "%02d:%02d:%02d" (int hours) (int minutes) (int seconds))))

(defn + [time duration]
  (.plus (t/instant time) duration))

(defn - [time duration]
  (.minus (t/instant time) duration))
