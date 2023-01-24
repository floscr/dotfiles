(ns monitor_brightness
  (:require
   [babashka.cli :as cli]
   [babashka.process :as bp]
   [lib.shell :refer [sh-lines]]
   [clojure.string :as str]))

;; Config ----------------------------------------------------------------------

(def displays {:external {:id 10}})

;; Helpers ---------------------------------------------------------------------

(defn display-brightness [display-id]
  (some->> (sh-lines (format "sudo ddcutil getvcp %d" display-id))
           (filter #(str/starts-with? % "VCP code"))
           (first)
           (re-find #"current value =\s+(\d+)")
           (last)
           (Integer/parseInt)))

;; Commands --------------------------------------------------------------------

(defn toggle-brightness! []
  (when-let [brightness (display-brightness (get-in displays [:external :id]))]
    (let [new-brightness (if (>= brightness 80) 50 25)]
      (bp/shell (format "ddcutil setvcp 10 %d" new-brightness))
      (println "Set the brightness to " new-brightness))))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["toggle"] :fn toggle-brightness!}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (-main)
  nil)
