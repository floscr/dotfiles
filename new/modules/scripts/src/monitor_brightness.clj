(ns monitor_brightness
  (:require
   [babashka.cli :as cli]
   [babashka.process :as bp]
   [clojure.string :as str]
   [lib.notifications :as notifications]
   [lib.num :as num]
   [lib.shell :as shell]))

;; Config ----------------------------------------------------------------------

(def displays {:external {:id 10}})

;; Helpers ---------------------------------------------------------------------

(defn display-brightness [display-id]
  (some->> (shell/lines (format "sudo ddcutil getvcp %d" display-id))
           (filter #(str/starts-with? % "VCP code"))
           (first)
           (re-find #"current value =\s+(\d+)")
           (last)
           (num/parse-int)))

(defn set-display-brightness! [display-id brightness & {:keys [brightness-label]}]
  (notifications/show (format "New monitor brightness: %s" (or brightness-label brightness)) {:clear-after-s 3})
  (bp/sh (format "sudo ddcutil setvcp %d %d" display-id brightness))
  (println "Set the brightness to: " brightness))

;; Commands --------------------------------------------------------------------

(defn toggle-brightness-cmd [_]
  (when-let [brightness (display-brightness (get-in displays [:external :id]))]
    (let [high [55 "High (55)"]
          low [25 "Low (25)"]
          new-brightness (if (>= brightness (first high)) low high)]
      (set-display-brightness! (get-in displays [:external :id]) (first new-brightness) {:brightness-label (last new-brightness)})
      (first new-brightness))))

(defn set-brightness-cmd [{:keys [opts]}]
  (set-display-brightness! (get-in displays [:external :id]) (:value opts)))

(defn set-max-brightness-cmd []
  (set-display-brightness! (get-in displays [:external :id]) 100))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["toggle"] :fn toggle-brightness-cmd}
   {:cmds ["max"] :fn set-max-brightness-cmd}
   {:cmds ["set"] :args->opts [:value]
    :coerce {:value :int} :fn set-brightness-cmd}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (display-brightness (get-in displays [:external :id]))
  (-main "toggle")
  (-main "max")
  (-main "set" "33")
  nil)
