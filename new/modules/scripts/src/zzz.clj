#!/usr/bin/env bb
(ns zzz
 (:require
  [babashka.cli :as cli]
  [babashka.process :as bp]
  [lib.emacs :as emacs]))

(def cli-opts
  {:dry-run {:alias :d
             :desc "Don't execute sleep."}})

(def restore-file "/tmp/my-zzz-restore-after-sleep")

(defn notify [msg]
  (print "Going to sleep...")
  (bp/shell (str "notify-send '" msg "'")))

(defn prepare
  ([] (prepare false))
  ([with-restore?]
   (when with-restore?
     (spit restore-file ""))
   (bp/shell {:continue true} "playerctl pause &")
   (emacs/run-cmd! "(org-clock-out)")))

(defn sleep [{:keys [opts] :as args}]
  (let [{:keys [dry-run]} opts]
    (notify " Going to sleep...")
    (prepare)
    (when-not dry-run
      (bp/shell "systemctl suspend"))))

(defn turn-off-display [{:keys [opts] :as args}]
  (let [{:keys [dry-run]} opts]
    (notify " Turning off display...")
    (prepare true)
    (when-not dry-run
      (bp/shell "xset dpms force off"))))

(defn help
  [_]
  (println
   (str "sleep\n" (cli/format-opts {:spec cli-opts}))))

(def main
  [{:cmds ["sleep"] :fn sleep :spec cli-opts}
   {:cmds [] :fn turn-off-display}])

(cli/dispatch main *command-line-args*)
