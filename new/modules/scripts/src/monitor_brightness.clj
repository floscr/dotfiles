(ns monitor_brightness
  (:require
   [babashka.cli :as cli]
   [babashka.process :as bp]
   [clojure.string :as str]
   [lib.notifications :as notifications]
   [lib.num :as num]
   [lib.shell :as shell]
   [lib.rofi :as rofi]))

;; Config ----------------------------------------------------------------------

(def displays {:external {:id 10}})

(def main-display-id (get-in displays [:external :id]))

;; Helpers ---------------------------------------------------------------------

(defn display-brightness [display-id]
  (some->> (shell/lines (format "ddcutil getvcp %d" display-id))
           (filter #(str/starts-with? % "VCP code"))
           (first)
           (re-find #"current value =\s+(\d+)")
           (last)
           (num/parse-int)))

(defn set-display-brightness!
  "Set the display `brightness` for monitor with `display-id`.
  If you want a special name for the notification to display pass `brightness-label`."
  [display-id brightness & {:keys [brightness-label]}]
  (notifications/show (format "New monitor brightness: %s" (or brightness-label brightness)) {:clear-after-s 3})
  (bp/sh (format "ddcutil setvcp %d %d" display-id brightness))
  (println "Set the brightness to: " brightness))

;; Commands --------------------------------------------------------------------

(defn toggle-brightness-cmd [_]
  (when-let [brightness (display-brightness main-display-id)]
    (let [high [55 "High (55)"]
          low [25 "Low (25)"]
          new-brightness (if (>= brightness (first high)) low high)]
      (set-display-brightness! main-display-id (first new-brightness) {:brightness-label (last new-brightness)})
      (first new-brightness))))

(defn set-brightness-cmd [{:keys [opts]}]
  (set-display-brightness! main-display-id (:value opts)))

(defn set-max-brightness-cmd []
  (set-display-brightness! main-display-id 100))

(defn rofi-brightness-cmd
  ([] (rofi-brightness-cmd {}))
  ([_]
   (some->> (range 0 101 10)
            (rofi/select)
            (set-display-brightness! main-display-id))))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["toggle"] :fn toggle-brightness-cmd}
   {:cmds ["rofi"] :fn rofi-brightness-cmd}
   {:cmds ["max"] :fn set-max-brightness-cmd}
   {:cmds ["set"]
    :args->opts [:value]
    :coerce {:value :int}
    :fn set-brightness-cmd}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (display-brightness (get-in displays [:external :id]))
  (-main "toggle")
  (-main "max")
  (-main "set" "33")
  (-main "rofi")
  nil)
