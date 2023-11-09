(ns org-attach
  (:require
   [babashka.fs :as fs]
   [babashka.process :as bp]
   [cats.core :as m]
   [cats.monad.exception :as exc]
   [clojure.string :as str]
   [lib.fs]
   [lib.shell]
   [lib.web]))

(def device-regex
  "Regex to find my device from the scanimage cli.

   The output looks something like this:
     device `v4l:/dev/video2' is a Noname Integrated Camera: Integrated I virtual device
     device `v4l:/dev/video0' is a Noname Integrated Camera: Integrated C virtual device
     device `epsonds:libusb:003:034' is a Epson ES-50 ESC/I-2"
  #"`(epsonds.+)'")

;; Helpers ---------------------------------------------------------------------

(defn failure [& {:keys [message] :as opts}]
  (exc/failure
   (merge {:kind :scan/error} opts)
   message))

(defn exc->error-message [exc]
  (let [err-map (-> (m/extract exc)
                    (Throwable->map))
        custom-err (get-in err-map [:via 0])
        msg (cond
              (:message custom-err) (:message custom-err)
              :else "Something failed")]
    msg))

(defn exc-print! [exc]
  (let [msg (if (exc/failure? exc)
              (exc->error-message exc)
              (m/extract exc))]
    (println "Error:" msg))
  exc)

;; Scanning Functions ----------------------------------------------------------

(defn find-device! []
  (m/mlet [devices (->> (lib.shell/sh-exc "scanimage -L")
                        (m/fmap #(str/split % #"\n")))
           device (or (some->> devices
                               (map #(re-find device-regex %))
                               (filter some?)
                               (first)
                               (second)
                               (exc/success))
                      (failure :kind :error/no-device
                               :message "Couldn't find scanner."
                               :info devices))]
    (m/return device)))

(comment
  (find-device!)
  nil)

(defn scan [& {:as opts}]
  (-> (m/mlet [device (find-device!)
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
       (exc-print!)
       (reset! a))

  (exc-print! @a)
  (print-error @a)

  (bp/shell "scanimage")
  nil)
