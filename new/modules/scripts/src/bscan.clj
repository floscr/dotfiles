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

(defn prn-verbose [{:keys [verbose?]} & msgs]
  (when verbose?
    (apply println (concat "i" msgs))))

(defn prn-debug
  ([msg] (prn-debug msg nil))
  ([msg x]
   (prn msg)
   x))

(defn failure [& {:keys [message] :as opts}]
  (exc/failure
   (merge {:kind :scan/error} opts)
   message))

(defn exc->error-message [exc opts]
  (let [err-map (-> (m/extract exc)
                    (Throwable->map))
        custom-err (get-in err-map [:via 0])
        msg (cond
              (:message custom-err) (:message custom-err)
              :else "Something failed")]
    msg))

(defn exc-print! [exc opts]
  (let [msg (if (exc/failure? exc)
              (exc->error-message exc opts)
              (m/extract exc))]
    (println "Error:" msg))
  exc)

(defn lookup-device
  "Look up device identifier via `device-regex` in output from 'scanimage -L' shell command.

  The output looks something like this:
  device `v4l:/dev/video2' is a Noname Integrated Camera: Integrated I virtual device
  device `v4l:/dev/video0' is a Noname Integrated Camera: Integrated C virtual device
  device `epsonds:libusb:003:034' is a Epson ES-50 ESC/I-2"
  [devices-str]
  (if-let [device-name (some->> devices-str
                                (map #(re-find device-regex %))
                                (filter some?)
                                (first)
                                (second))]
    (exc/success device-name)
    (failure :kind :error/no-device
             :message "Couldn't find scanner."
             :verbose-message devices-str)))

;; Scanning Functions ----------------------------------------------------------

(defn find-device! [{:keys [verbose? debug?] :as opts}]
  (prn-verbose opts "Looking up device...")
  (m/mlet [devices (->> (lib.shell/sh-exc "scanimage -L")
                        (m/fmap #(str/split % #"\n")))
           device (lookup-device devices)]
    (prn-verbose opts "Found device:" device)
    (m/return device)))

(comment
  (find-device! {:verbose? true})
  nil)

(defn scan [& {:as opts}]
  (-> (m/mlet [device (find-device! opts)
               scan-temp-file (-> (fs/create-temp-file {:prefix "scan-"
                                                        :suffix ".pnm"})
                                  (exc/success))
               _ (lib.shell/sh-exc ["scanimage"
                                    "--device" device
                                    "--mode" "Color"
                                    "--resolution" "300"
                                    "--format" "pnm"
                                    "--output" scan-temp-file])]
        (m/return scan-temp-file))))

(comment
  (defonce a (atom nil))

  (->> (scan :debug? true)
       (exc-print! {})
       (reset! a))

  (exc-print! @a)
  (print-error @a)

  (bp/shell "scanimage")
  nil)
