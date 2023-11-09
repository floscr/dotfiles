(ns bscan
  (:require
   [babashka.fs :as fs]
   [babashka.process :as bp]
   [cats.core :as m]
   [cats.monad.exception :as exc]
   [clojure.string :as str]
   [lib.fs]
   [lib.shell]
   [lib.web]))

;; Config ----------------------------------------------------------------------

(def device-regex
  "Regex to find my device from the scanimage cli."
  #"`(epsonds.+)'")

;; Helpers ---------------------------------------------------------------------

(defn debug-prn [{:keys [debug?] :as _opts} & msgs]
  (when debug?
    (apply println (concat "i" msgs))))

(defn failure [& {:keys [message] :as opts}]
  (exc/failure
   (merge {:kind :scan/error} opts)
   message))

(defn exc->error-message [{:keys [verbose?] :as _opts} exc]
  (let [err-map (-> (m/extract exc)
                    (Throwable->map))
        custom-err (get-in err-map [:via 0])
        verbose-message (when verbose? (get-in err-map [:data :verbose-message]))
        msg (cond
              (:message custom-err) (cond-> (:message custom-err)
                                      verbose-message (str "\n\n" (str/join "\n" verbose-message)))
              :else "Something failed")]
    msg))

(defn exc-print! [opts exc]
  (let [msg (if (exc/failure? exc)
              (exc->error-message opts exc)
              (m/extract exc))]
    (if (exc/failure? exc)
      (println "Error:" msg)
      (println msg)))
  exc)

(comment
  (exc-print! @a {})
  nil)


(defn lookup-device
  "Look up device identifier via `device-regex` in output from 'scanimage -L' shell command.

  The output looks something like this:
  device `v4l:/dev/video2' is a Noname Integrated Camera: Integrated I virtual device
  device `v4l:/dev/video0' is a Noname Integrated Camera: Integrated C virtual device
  device `epsonds:libusb:003:034' is a Epson ES-50 ESC/I-2"
  [opts devices-str]
  (if-let [device-name (some->> devices-str
                                (map #(re-find device-regex %))
                                (filter some?)
                                (first)
                                (second))]
    (exc/success device-name)
    (failure :kind :error/no-device
             :message "Couldn't find scanner."
             :verbose-message (when (seq devices-str)
                                (into ["Found devices:"] devices-str)))))

;; Scanning Functions ----------------------------------------------------------

(defn find-device! [{:keys [verbose? debug?] :as opts}]
  (m/mlet [_ (exc/success (debug-prn opts "Looking up device..."))
           devices (->> (lib.shell/sh-exc "scanimage -L")
                        (m/fmap #(str/split % #"\n")))
           device (lookup-device opts devices)]
    (debug-prn opts "Found device:" device)
    (m/return device)))

(comment
  (reset! a (find-device! {:debug? true}))

  (exc-print! @a {:verbose? true})
  nil)

(defn scan [{:as opts}]
  (m/mlet [device (find-device! opts)
           scan-temp-file (-> (fs/create-temp-file {:prefix "bscan-"
                                                    :suffix ".pnm"})
                              (exc/success))
           _ (lib.shell/sh-exc ["scanimage"
                                "--device" device
                                "--mode" "Color"
                                "--resolution" "300"
                                "--format" "pnm"
                                "--output" scan-temp-file])]
    (m/return scan-temp-file)))

(comment
  (defonce a (atom nil))


  (let [opts {:debug? true
              :verbose? true}]
    (->> (scan opts)
         (exc-print! opts)))


  (bp/shell "scanimage")
  nil)
