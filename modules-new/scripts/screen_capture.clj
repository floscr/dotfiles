#!/usr/bin/env bb
(ns screen-capture
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :as bp]
            [clojure.string :as str]
            [clojure.java.io :as io]))

;; Variables -------------------------------------------------------------------

(def cli-command "screen_capture")

(def stop-file "/tmp/my-screen-capture-pid")

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

(defmacro bold [str]
  `(str "\033[1m" ~str "\033[0m"))

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
  (println opts)
  (let [{:keys [extension]
         :or {extension "png"}} opts
        path (or (:file opts)
                 (filename (:static default-dirs) extension))]
    (bp/shell {:out path} "maim --delay=0.1 --hidecursor --select --quiet")
    (bp/shell "notify-send" "Screenshot saved" (format "Copied to clipboard\n%s" path))
    (bp/shell "xclip -selection clipboard -t" (format "image/%s" extension) path)
    path))

(defn capture-animated! [{:keys [opts]}]
  (let [{:keys [countdown]
         :or {countdown 3}} opts
        ext "mp4"
        path (or (:file opts)
                 (filename (:animated default-dirs) ext))
        {:keys [width height x y]} (get-x-rect)]
    (when width
      (when (pos? countdown)
        (notification-countdown! countdown))
      (bp/process "ffmpeg"
                  "-y" ; Ignore globals
                  "-f" "x11grab"
                  "-show_region" "1"
                  "-s" (format "%dx%d" width height)
                  "-i" (format ":0.0+%d,%d" x y)
                  "-framerate" "30"
                  path))))

(defn toggle-capture-animated! [args]
  (let [file stop-file]
    (if (fs/exists? file)
      (let [[pid] (fs/read-all-lines file)]
        (fs/delete file)
        (bp/shell {:continue true} "kill" pid))
      (let [proc (capture-animated! args)
            pid (-> (:proc proc)
                    (.pid)
                    (str))]
        (fs/write-lines file [pid])
        @proc))))

(defn help
  [_]
  (println
   "Capture a screen area\n"
   "\n"

   (bold "USAGE\n")
   cli-command " <command> <file> [flags]\n"
   "\n"

   (bold "COMMANDS\n")

   "static: Capture a screenshot in region\n"
   "png: Capture png\n"
   "jpg: Capture jpg\n"
   "\n"

   "record: Record a screen region\n"
   "mp4: Capture mp4\n"))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["static"] :args->opts [:file] :fn capture-static!}
   {:cmds ["png"] :args->opts [:file] :exec-args {:extension "png"} :fn capture-static!}
   {:cmds ["jpg"] :args->opts [:file] :exec-args {:extension "jpg"} :fn capture-static!}

   {:cmds ["record"] :args->opts [:file] :fn toggle-capture-animated!}
   {:cmds ["toggle"] :args->opts [:file] :fn toggle-capture-animated!}
   {:cmds ["mp4"] :args->opts [:file] :fn toggle-capture-animated!}

   {:cmds [] :fn help}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

;; Testing ---------------------------------------------------------------------

(comment
  ;; {:cmds (single), :args (), :rest-cmds (), :opts {}, :dispatch [single]}
  (capture-static! {:opts {:ext "jpg"}})

  (-main "single")
  (-main "png")
  (-main "jpg")

  (-main "toggle")
  (-main "single" "/tmp/example.png")

  (def proc (capture-animated! {:opts {:countdown 0}}))

  (toggle-capture-animated! {:opts {:countdown 0}})

  ;; {:cmds (single sdfsdf), :args (), :rest-cmds (), :opts {:file sdfsdf}, :dispatch [single]}
  (cli/parse-)
  nil)
