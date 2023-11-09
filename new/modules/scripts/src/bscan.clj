(ns bscan
  (:require
   [babashka.fs :as fs]
   [babashka.process :as bp]
   [cats.core :as m]
   [cats.monad.exception :as exc]
   [clojure.string :as str]
   [lib.fp]
   [lib.fs]
   [lib.shell]
   [lib.web]))

;; Config ----------------------------------------------------------------------

(def device-regex
  "Regex to find my device from the scanimage cli."
  #"`(epsonds.+)'")

;; Helpers ---------------------------------------------------------------------

(defn debug-str [msgs]
  (concat "i" msgs))

(defn debug-prn [{:keys [debug?] :as _opts} & msgs]
  (apply println (debug-str msgs)))

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

(defn execute-pipeline
  "Executes a pipeline which is a list of functions that return a monadic value.
  These functions take as arguments (fn [opts state]).
  Whenever a part in the pipline fails the execution is halted.

  The pipeline can also be filled with debugging messages for vebose printing of commands.
  ['My string' :foo str]
  Here the vector has the debugging message as the first string, and some accessors on the current state.
  Which would print for the current state of: {:foo 1}
  'My string 1'"
  [opts pipeline]
  (reduce
   (fn [acc-mv cur]
     (cond
       (fn? cur) (m/bind acc-mv #(cur opts %))
       (vector? cur) (m/bind acc-mv (fn [state]
                                      (let [[msg & selectors] cur
                                            state-msg (when (seq? selectors)
                                                        (lib.fp/apply-fns state selectors))
                                            msgs (if state-msg [msg state-msg] [msg])]
                                        (apply println (debug-str msgs)))
                                      acc-mv))))
   (exc/success {})
   pipeline))

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

(defn find-device! [opts _state]
  (m/mlet [devices (->> (lib.shell/sh-exc "scanimage -L")
                        (m/fmap #(str/split % #"\n")))
           device (lookup-device opts devices)]
    (m/return {:device device})))

(defn scan! [opts {:keys [device] :as state}]
  (m/mlet [scan-temp-file (-> (fs/create-temp-file {:prefix "bscan-"
                                                    :suffix ".pnm"})
                              (exc/success))
           _ (lib.shell/sh-exc ["scanimage"
                                "--device" device
                                "--mode" "Color"
                                "--resolution" "300"
                                "--format" "pnm"
                                "--output" scan-temp-file])]
    (m/return (assoc state :device device
                           :scanned-file scan-temp-file))))

(defn process! [opts {:keys [scanned-file] :as state}]
  (let [processed-file (lib.fs/rename-extension scanned-file "tiff")]
    (m/mlet [_ (lib.shell/sh-exc ["unpaper" scanned-file processed-file])]
      (m/return (assoc state :processed-file processed-file)))))

(defn ocr! [_opts {:keys [processed-file ocr-opts] :as state}]
  (let [{:keys [language format]
         :or {language "deu"
              format "pdf"}} ocr-opts
        [output] (fs/split-ext processed-file)]
    (m/mlet [_ (lib.shell/sh-exc ["tesseract" "-l" language
                                  processed-file
                                  output
                                  "pdf"])]
      (m/return (assoc state :ocr-file output)))))

(comment
  (ocr! opts (m/extract @a))
  nil)

(def pipeline [find-device!
               scan!
               process!
               ocr!])

(def verbose-pipeline [["Looking up device..."] find-device! ["Found device:" :device]
                       ["Scanning document..."] scan! ["Scanned document" :scanned-file str]
                       ["Processing scanned file..."] process! ["Processed document" :processed-file str]])

(comment
  (defonce a (atom nil))

  (def opts {:debug? true
             :verbose? true})

  (reset! a (execute-pipeline opts verbose-pipeline))

  (ocr! opts (m/extract @a))

  (bp/shell "scanimage")
  nil)
