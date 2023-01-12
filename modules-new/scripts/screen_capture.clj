#!/usr/bin/env bb
(ns screen-capture
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :as bp]
            [clojure.string :as str]))

;; Variables -------------------------------------------------------------------

(def default-dirs {:static "~/Media/Screencapture"
                   :animated "~/Media/Screenrecording"})

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

(defn notification-countdown!
  "Shell like countdown displaying a notification every `update-every` second for `seconds` duration.
  Uses 1 second as the default `update-every` arg."
  ([seconds]
   (notification-countdown! 1 seconds))
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

(defn parse-int [str]
  (Integer/parseInt str))

(defn keep-> [x pred]
  (when (pred x) x))

(defn keep-> [pred x]
  (when (pred x) x))

(defn discard-> [x pred]
  (when-not (pred x) x))

(defn discard->> [pred x]
  (when-not (pred x) x))

(defn get-x-rect []
  (when-let [[width height x y]
             (some-> (bp/shell {:out :string
                                :continue true} "slop -nof '%w %h %x %y'")
                    :out
                    (discard-> str/blank?)
                    (str/split #" ")
                    (#(map parse-int %)))]
    {:width width
     :height height
     :x x
     :y y}))

;; Commands --------------------------------------------------------------------

(defn capture-static! [{:keys [opts]}]
  (let [ext "png"
        path (or (:file opts)
                 (filename (:static default-dirs) ext))]
    (bp/shell {:out path} "maim --delay=0.1 --hidecursor --select --quiet")
    (bp/shell "notify-send" "Screenshot saved" (format "Copied to clipboard\n%s" path))
    (bp/shell "xclip -selection clipboard -t" (format "image/%s" ext) path)
    path))

(defn capture-animated! [{:keys [opts]}]
  (let [ext "mp4"
        path (or (:file opts)
                 (filename (:animated default-dirs) ext))
        {:keys [width height x y]} (get-x-rect)]
    (when width
      (notification-countdown! 3)
      (bp/shell "ffmpeg" "-y"
                "-f" "x11grab"
                "-show_region" "1"
                "-s" (format "%dx%d" width height)
                "-i" (format ":0.0+%d,%d" x y)
                "-framerate" "30"
                path))))

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

  (capture-animated! {})


  ;; {:cmds (single sdfsdf), :args (), :rest-cmds (), :opts {:file sdfsdf}, :dispatch [single]}
  (cli/parse-)
  nil)
