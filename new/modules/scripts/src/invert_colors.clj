#!/usr/bin/env bb
(ns invert-colors
  (:require
   [babashka.cli :as cli]
   [babashka.process :as bp]
   [clojure.string :as str]))

(defn x-window-id! []
  (-> (bp/sh "xdotool getwindowfocus") :out (str/trim)))

(defn invert-colors! [{:keys [opts]}]
  (let [{:keys [window-id]
         :or {window-id (x-window-id!)}} opts
        inverted? (some->> (bp/sh (format "xprop -id %s" window-id))
                           :out
                           (str/split-lines)
                           (filter #(str/includes? % "TAG_INVERT"))
                           (first)
                           (last)
                           (= \1))
        flag (if inverted? 0 1)]
    (bp/sh (format "xprop -f TAG_INVERT 8c -set TAG_INVERT %d -id %s" flag window-id))))

(def table
  [{:cmds [] :args->opts [:window-id] :fn invert-colors!}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (-main)
  nil)
