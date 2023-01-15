#!/usr/bin/env bb
(ns lib
 (:require [babashka.cli :as cli]
           [babashka.fs :as fs]
           [babashka.process :as bp]))

(defn notification-countdown
  "Shell like countdown displaying a notification every `update-every` second for `seconds` duration.
  Uses 1 second as the default `update-every` arg."
  ([seconds]
   (notification-countdown 1 seconds))
  ([update-every seconds]
   (let [update-every-ms (* update-every 1000)

         repeater (->> (/ seconds update-every)
                       (+ 1)
                       (range 1)
                       (reverse)
                       (vec))]
     (doseq [i repeater]
       (bp/shell "notify-send -u critical" "-t" update-every-ms (format "Recording in %d" i))
       (bp/shell "sleep" update-every)))))

(comment
  (doall
    (notification-countdown 1 5)
    (notification-countdown 5)
    (println "foo"))
  nil)
