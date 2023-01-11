#!/usr/bin/env bb
(ns screen-capture
 (:require [babashka.cli :as cli]
           [babashka.fs :as fs]
           [babashka.process :as bp]))

(def default-dirs {:single "~/Media/Screencapture"})

(def cli-opts {:dir {:alias :d
                     :require true}})

(defn filename-date []
  (.format (java.text.SimpleDateFormat. "yyyy-MM-dd-HH-mm:ss") (new java.util.Date)))

(defn filename [dir ext]
  (let [date (filename-date)
        path (-> (fs/path dir (str date "." ext))
                 (fs/expand-home))]
    (str path)))

(defn capture-single! [{:keys [opts]}]
  (let [ext  "png"
        path (or
              (:file opts)
              (filename (:single default-dirs) ext))]
    (bp/shell {:out path} "maim --delay=0.1 --hidecursor --select --quiet")
    (bp/shell "notify-send" "Screenshot saved" (format "Copied to clipboard\n%s" path))
    (bp/shell "xclip -selection clipboard -t" (format "image/%s" ext) path)
    path))

(defn help
  [_]
  (println
   "screen-capture\n"
   "single Capture a single screenshot\n"))

(def table
  [{:cmds ["single"] :args->opts [:file] :fn capture-single!}
   {:cmds [] :fn help}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (-main "single")
  ;; {:cmds (single), :args (), :rest-cmds (), :opts {}, :dispatch [single]}
  (-main "single" "/tmp/example.png")
  ;; {:cmds (single sdfsdf), :args (), :rest-cmds (), :opts {:file sdfsdf}, :dispatch [single]}
  (cli/parse-)
  nil)
