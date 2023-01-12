#!/usr/bin/env bb
(ns screen-capture
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :as bp]))

;; Variables -------------------------------------------------------------------

(def default-dirs {:single "~/Media/Screencapture"})

(def cli-opts {:dir {:alias :d
                     :require true}})

;; Helpers ---------------------------------------------------------------------

(defn filename-date []
  (.format (java.text.SimpleDateFormat. "yyyy-MM-dd-HH-mm:ss") (new java.util.Date)))

(defn filename [dir ext]
  (let [date (filename-date)
        path (-> (fs/path dir (str date "." ext))
                 (fs/expand-home))]
    (str path)))

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

;; Commands --------------------------------------------------------------------

(defn capture-static! [{:keys [opts]}]
  (let [ext "png"
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

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["single"] :args->opts [:file] :fn capture-static!}
   {:cmds [] :fn help}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

;; Testing ---------------------------------------------------------------------

(comment
  (-main "single")
  ;; {:cmds (single), :args (), :rest-cmds (), :opts {}, :dispatch [single]}
  (-main "single" "/tmp/example.png")
  ;; {:cmds (single sdfsdf), :args (), :rest-cmds (), :opts {:file sdfsdf}, :dispatch [single]}
  (cli/parse-)
  nil)
