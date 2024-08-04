#!/usr/bin/env bb
(ns screen-capture
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [babashka.process :as bp]
   [cheshire.core :as json]
   [clojure.string :as str]
   [lib.clipboard :refer [set-clip]]
   [lib.fp :as fp]
   [lib.fs]
   [lib.num :as num]
   [lib.shell :as lib.sh]))

;; Variables -------------------------------------------------------------------

(def cli-command "screen_capture")

(def stop-file "/tmp/my-screen-capture-pid")

(def default-dirs {:static "~/Media/Screencapture"
                   :animated "~/Media/Screenrecording"})

(def cli-opts {:dir {:alias :d
                     :require true}})

(def screenkey-opts {:height 400})

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

(defn get-x-rect []
  (when-let [[width height x y]
             (some-> (bp/shell {:out :string
                                :continue true} "slop -nof '%w %h %x %y'")
                     :out
                     (fp/discard-> str/blank?)
                     (str/split #" ")
                     (#(map num/parse-int %)))]
    {:width width
     :height height
     :x x
     :y y}))

;; Commands --------------------------------------------------------------------

(defn divisible-by-two? [n]
  (= 0 (rem n 2)))

(defn divisible-by-two [n]
  (if (divisible-by-two? n)
    n
    (inc n)))

(defn capture-static! [{:keys [opts]}]
  (let [{:keys [extension]
         :or {extension "png"}} opts
        path (or (:file opts)
                 (filename (:static default-dirs) extension))]
    (bp/shell {:out path} "maim --delay=0.1 --hidecursor --select --quiet")
    (bp/shell "notify-send" "Screenshot saved" (format "Copied to clipboard\n%s" path))
    (bp/shell "xclip -selection clipboard -t" (format "image/%s" extension) path)
    path))

(defn capture-animated! [{:keys [opts]}]
  (let [{:keys [countdown screenkey]
         :or {countdown 1}} opts
        ext "mp4"
        path (or (:file opts)
                 (filename (:animated default-dirs) ext))
        {:keys [width height x y]} (get-x-rect)
        cmd ["ffmpeg"
             "-y" ; Ignore globals
             "-f" "x11grab"
             "-show_region" "1"
             "-s" (format "%dx%d" (divisible-by-two width) (divisible-by-two height))
             "-i" (format ":0.0+%d,%d" x y)
             "-framerate" "30"
             "-vf" "format=yuv420p"
             path]]
    (when width
      (when (pos? countdown)
        (notification-countdown! countdown))
      (when screenkey
        (bp/process "screenkey" "--geometry" (format "%dx%d+%d+%d"
                                                     width
                                                     (:height screenkey-opts)
                                                     x
                                                     (+ y (- height (:height screenkey-opts))))))
      [path (bp/process cmd)])))

(defn stop-file-pid []
  (some->> (when (fs/exists? stop-file)
             (fs/read-all-lines stop-file))
           (first)))

(defn process-running? [pid]
  (when pid
    (-> (bp/sh ["ps" pid])
        (:exit)
        (zero?))))

(defn remove-stop-file! []
  (let [pid (stop-file-pid)
        running? (process-running? pid)]
    (when running?
      (bp/shell {:continue true} "kill" "-2" pid)
      (fs/delete stop-file)
      true)))

(defn trim-ending-shortcut-length! [path]
  (when-let [duration (some-> (bp/sh "ffprobe"
                                     "-v" "quiet"
                                     "-show_format"
                                     "-of" "default=noprint_wrappers=1:nokey=1"
                                     "-print_format" "json"
                                     "-i" "/home/floscr/Media/Screenrecording/2024-05-22-17-43:35.mp4")
                              :out
                              (json/parse-string)
                              (get-in ["format" "duration"])
                              (num/safe-parse-float))]
    (let [temp-file-path (lib.fs/fs-temp-file-path {:suffix ".mp4"})]
      (prn ["ffmpeg"
            "-i" path
                 ;; Change the duration
            "-t" (max 0.75 (- duration 0.75))
                 ;; Copy the codec
            "-c" "copy"
            temp-file-path])
      (bp/shell ["ffmpeg"
                 "-i" path
                 ;; Change the duration
                 "-t" (max 0.75 (- duration 3))
                 ;; Copy the codec
                 "-c" "copy"
                 temp-file-path])
      (fs/delete path)
      (fs/copy temp-file-path path))))

(comment
  (let [path "/home/floscr/Media/Screenrecording/2024-05-22-17-43:35.mp4"]
    (trim-ending-shortcut-length! path))
  nil)

(defn toggle-capture-animated! [{:keys [opts] :as args}]
  (when-not (remove-stop-file!)
    (let [[path proc] (capture-animated! args)
          pid (some-> (:proc proc)
                      (.pid)
                      (str))]
      (when proc
        (fs/write-lines stop-file [pid])
        @proc
        (when (:screenkey opts)
          (trim-ending-shortcut-length! path))
        (bp/shell (format "notify-send 'Recording saved to %s\nCopied path to clipboard!'" path))
        (set-clip path)
        ;; Keep at end, otherwise it kills the whole process?
        (when (:screenkey opts)
          (bp/sh "pkill -f screenkey"))))))

(defn help
  [_]
  (println
   "Capture a screen area\n"
   "\n"

   (lib.sh/bold "USAGE\n")
   cli-command " <command> <file> [flags]\n"
   "\n"

   (lib.sh/bold "COMMANDS\n")

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

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

;; Testing ---------------------------------------------------------------------

(comment
  (-> (stop-file-pid)
      (process-running?))
  nil)

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
