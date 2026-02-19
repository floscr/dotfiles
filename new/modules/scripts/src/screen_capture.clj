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
   [lib.shell :as lib.sh]
   [lib.x11]))

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
    (dec n)))

(defn capture-static! [{:keys [opts]}]
  (let [{:keys [extension]
         :or {extension "png"}} opts
        path (or (:file opts)
                 (filename (:static default-dirs) extension))]
    (fs/create-dirs (fs/parent path))
    (bp/shell {:out path} "maim --delay=0.1 --hidecursor --select --quiet")
    (bp/shell "notify-send" "Screenshot saved" (format "Copied to clipboard\n%s" path))
    (bp/shell "xclip -selection clipboard -t" (format "image/%s" extension) path)
    path))

(defn capture-animated! [{:keys [opts]}]
  (let [{:keys [countdown screenkey audio]
         :or {countdown 1}} opts
        ext "mp4"
        path (or (:file opts)
                 (filename (:animated default-dirs) ext))
        _ (fs/create-dirs (fs/parent path))
        {:keys [width height x y] :as rect} (get-x-rect)
        resolution (lib.x11/display-resolution)
        safe-width (-> (- (min (+ x width) (dec (:width resolution))) x)
                       (divisible-by-two))
        safe-height (-> (- (min (+ y height) (dec (:height resolution))) y)
                        (divisible-by-two))
        _ (spit "/tmp/my-screen-capture-rect" (format "Rectangle %d %d %d %d" x y safe-width safe-height))
        cmd (vec (concat ["ffmpeg"
                          "-xerror"
                          "-y"] ; Ignore globals
                         ;; Audio input (if enabled) - must come BEFORE video input
                         (when audio
                           ["-f" "pulse"
                            "-i" "default"])
                         ;; Video input
                         ["-f" "x11grab"
                          "-show_region" "1"
                          "-s" (format "%dx%d" safe-width safe-height)
                          "-i" (format ":0.0+%d,%d" x y)
                          "-framerate" "30"]
                         ;; Codec options
                         ["-c:v" "libx264"
                          "-preset" "ultrafast"
                          "-crf" "23"
                          "-vf" "format=yuv420p"]
                         ;; Audio codec (if audio is enabled)
                         (when audio
                           ["-c:a" "aac"
                            "-b:a" "128k"
                            "-ac" "2"])  ; Stereo audio
                         [path]))
        _ (spit "/tmp/my-screen-capture-cmd" (str/join " " cmd))]
    (when width
      (when (pos? countdown)
        (notification-countdown! countdown))
      (when screenkey
        (bp/process "screenkey" "--geometry" (format "%dx%d+%d+%d"
                                                     width
                                                     (:height screenkey-opts)
                                                     x
                                                     (+ y (- height (:height screenkey-opts))))))
      {:path path
       :process (bp/process cmd)})))

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

(def ffmpeg-success-exit-codes
  "Toggling ffmpeg with a shortcut will never result in a 0 exit code, so we accept `255` exit code."
  #{0 255})

(defn interactive-terminal?
  "Check if we're running in an interactive terminal (TTY).
  Checks stdout (fd 1) instead of stdin since stdin might not be connected in bb scripts."
  []
  (or 
   ;; Check if stdout is a TTY
   (-> (bp/sh {:continue true} "test -t 1")
       :exit
       zero?)
   ;; Fallback: check if we have a TERM environment variable and no DISPLAY-less environment
   (and (System/getenv "TERM")
        (not= (System/getenv "TERM") "dumb"))))

(defn wait-for-keypress-async!
  "Start a background thread that waits for user to press 'q' or ESC to stop recording.
  When key is pressed, kills the process by PID."
  [pid]
  (future
    (try
      (bp/shell "stty -icanon -echo")
      (loop []
        (let [k (.read System/in)
              char (case k
                     27 :esc      ; ESC key
                     113 \q       ; 'q' key
                     nil)]
          (if (#{:esc \q} char)
            (do
              (bp/shell "stty icanon echo")
              (bp/shell {:continue true} "kill" "-2" pid)
              (println "\n⏹️  Stopping recording..."))
            (recur))))
      (catch Exception e
        (bp/shell "stty icanon echo")
        (println "Error waiting for keypress:" (.getMessage e))))))

(defn toggle-capture-animated! [{:keys [opts] :as args}]
  (when-not (remove-stop-file!)
    (let [{:keys [path process] :as _result} (capture-animated! args)
          pid (some-> (:proc process)
                      (.pid)
                      (str))
          interactive? (interactive-terminal?)]
      (spit "/tmp/my-screen-capture-debug" (format "interactive: %s, pid: %s, process: %s" interactive? pid (some? process)))
      (when process
        (fs/write-lines stop-file [pid])
        ;; If interactive terminal, start background thread to listen for 'q' keypress
        (when interactive?
          (println "\n🔴 Recording in progress...")
          (println "Press 'q' or ESC to stop, or run 'screen_capture toggle' again.\n")
          (wait-for-keypress-async! pid))
        (let [{:keys [exit err]} @process
              success? (get ffmpeg-success-exit-codes exit)]
          (if success?
            (do
              (when (:screenkey opts)
                (trim-ending-shortcut-length! path))
              (if interactive?
                (println (format "✅ Recording saved to %s\nCopied path to clipboard!" path))
                (bp/shell (format "notify-send 'Recording saved to %s\nCopied path to clipboard!'" path)))
              (set-clip path))
            (let [err-file (fs/create-temp-file)]
              (spit (str err-file) (slurp err))
              (if interactive?
                (println (format "❌ ERROR during recording\nError output saved to: %s" err-file))
                (bp/shell "notify-send" "-u" "critical" "ERROR during recording" (str "Error output saved to: " err-file)))))
          ;; Keep at end, otherwise it kills the whole process?
          (when (:screenkey opts)
            (bp/sh "pkill -f screenkey")))))))

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
   "mp4: Capture mp4\n"
   "toggle: Toggle recording on/off\n"
   "\n"

   (lib.sh/bold "FLAGS (for record/mp4/toggle)\n")
   "--audio: Include audio capture from default microphone\n"
   "--screenkey: Show keyboard input overlay\n"
   "--countdown <seconds>: Countdown before starting (default: 1)\n"
   "\n"

   (lib.sh/bold "STOPPING RECORDINGS\n")
   "Interactive mode (CLI): Press 'q' or ESC to stop and save\n"
   "Toggle mode (rofi/launcher): Call the command again to stop\n"))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["static"] :args->opts [:file] :fn capture-static!}
   {:cmds ["png"] :args->opts [:file] :exec-args {:extension "png"} :fn capture-static!}
   {:cmds ["jpg"] :args->opts [:file] :exec-args {:extension "jpg"} :fn capture-static!}

   {:cmds ["record"] 
    :args->opts [:file] 
    :opts {:audio {:coerce :boolean}
           :screenkey {:coerce :boolean}
           :countdown {:coerce :long :default 1}}
    :fn toggle-capture-animated!}
   {:cmds ["toggle"] 
    :args->opts [:file] 
    :opts {:audio {:coerce :boolean}
           :screenkey {:coerce :boolean}
           :countdown {:coerce :long :default 1}}
    :fn toggle-capture-animated!}
   {:cmds ["mp4"] 
    :args->opts [:file] 
    :opts {:audio {:coerce :boolean}
           :screenkey {:coerce :boolean}
           :countdown {:coerce :long :default 1}}
    :fn toggle-capture-animated!}

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

  (def process (capture-animated! {:opts {:countdown 0}}))

  (toggle-capture-animated! {:opts {:countdown 0}})

  ;; {:cmds (single sdfsdf), :args (), :rest-cmds (), :opts {:file sdfsdf}, :dispatch [single]}
  nil)
