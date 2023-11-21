#!/usr/bin/env bb
(ns bhotplug
  (:require
    [clojure.string :as str]
    [babashka.cli :as cli]
    [babashka.process :as bp]))

;; Helpers ---------------------------------------------------------------------

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

(defn connect-internal! []
  (bp/sh ["xrandr"
          "--output" "VIRTUAL1" "--off"
          "--output" "DP-1" "--off"
          "--output" "DP-3" "--off"
          "--output" "DP-1" "--off"
          "--output" "HDMI1" "--off"
          "--output" "HDMI2" "--off"
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

(defn on-connect! []
  (bp/sh "xsetroot -cursor_name left_ptr")
  (bp/sh "/etc/profiles/per-user/floscr/bin/emacsclient -e \"(my-ui|adjust-ui-to-display)\"")
  (bp/sh "/etc/profiles/per-user/floscr/bin/reloadWallpaper")
  (bp/sh "xrdb -merge /home/floscr/.config/xtheme/80-dpi")
  (bp/sh "/etc/profiles/per-user/floscr/bin/xmonadctl restart"))

;; Commands --------------------------------------------------------------------

(defn hotplug-cmd [_opts]
  (let [outputs (connected-outputs)]
    (cond
      (outputs "DP-3") (do
                         (connect-lg!)
                         (bp/sh "systemctl --user stop picom.service"))
      :else (do
              (connect-internal!)
              (bp/sh "systemctl --user start picom.service")))
    (on-connect!)))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds [] :fn hotplug-cmd}])

(defn -main [& args]
  (cli/dispatch table args))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
