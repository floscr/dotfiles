#!/usr/bin/env bb
(ns bhotplug
  (:require
   [babashka.cli :as cli]
   [babashka.process :as bp]
   [clojure.string :as str]
   [lib.emacs :as emacs]))

;; Helpers ---------------------------------------------------------------------

(defn parse-refresh-rate [xrandr-output]
  (->> (str/split-lines (:out xrandr-output))
       (filter #(re-find #" connected" %))
       (map #(str/split % #"\s+"))
       (mapcat rest)
       (filter #(re-find #"\*\([\d\.]+\)" %))
       (map #(re-find #"\(\d+\.\d+\)" %))
       (map #(str/replace % #"\(|\)" ""))
       (filter number?)
       (map str)
       (map #(Float/parseFloat %))))

(defn connect-lg! []
  (bp/sh ["xrandr"
          "--output" "eDP-1" "--off"
          "--output" "DP-3"
          "--primary"
          "--dpi" "110"
          "--panning" "3840x2160"
          "--mode" "3840x2160"
          "--pos" "0x0"
          "--rotate" "normal"
          "--auto"]))

(defn connect-hdmi-tv! []
  (bp/sh ["xrandr"
          "--output" "eDP-1" "--off"
          "--output" "HDMI-1"
          "--primary"
          "--dpi" "96"
          "--mode" "1920x1080"
          "--pos" "0x0"
          "--rotate" "normal"
          "--auto"]))

(defn connect-internal! []
  (bp/sh ["xrandr"
          "--output" "VIRTUAL1" "--off"
          "--output" "DP-1" "--off"
          "--output" "DP-3" "--off"
          "--output" "DP-1" "--off"
          "--output" "HDMI-1" "--off"
          "--output" "HDMI-2" "--off"
          "--output" "eDP-1"
          "--primary"
          "--dpi" "92"
          "--auto"]))

(defn connected-outputs []
  (->> (bp/sh "xrandr")
       :out
       (str/trim)
       (str/split-lines)
       (eduction
        (filter #(re-find #" connected" %))
        (map #(str/split % #"\s+"))
        (map first))
       (into #{})))

(comment
  (connected-outputs)
  nil)


(defn usbc-display-connected? []
  (some? (get (connected-outputs) "DP-3")))

(defn hdmi-tv-connected? []
  (some? (get (connected-outputs) "HDMI-1")))

(comment
  (connected-outputs)
  (usbc-display-connected?)
  (hdmi-tv-connected?)
  nil)

(defn on-connect! []
  (bp/sh "xsetroot -cursor_name left_ptr")
  (emacs/run-cmd! "(my-ui|adjust-ui-to-display)")
  (bp/sh "/etc/profiles/per-user/floscr/bin/reloadWallpaper")
  (bp/sh "xrdb -merge /home/floscr/.config/xtheme/80-dpi")
  (bp/sh "/etc/profiles/per-user/floscr/bin/xmonadctl restart"))

;; Commands --------------------------------------------------------------------

(defn hotplug-cmd [_opts]
  (let [outputs (connected-outputs)]
    (cond
      (outputs "DP-3")
      (do (connect-lg!)
          (bp/sh "systemctl --user stop picom.service")
          (println "Connecting to LG External Display."))
      (outputs "HDMI-1")
      (do (connect-hdmi-tv!)
          (bp/sh "systemctl --user stop picom.service")
          (println "Connecting to HDMI TV for video playback."))
      :else
      (do (connect-internal!)
          (bp/sh "systemctl --user start picom.service")
          (println "Disconnecting displays, activating internal display.")))
    (on-connect!)))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds [] :fn hotplug-cmd}])

(defn -main [& args]
  (cli/dispatch table args))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
