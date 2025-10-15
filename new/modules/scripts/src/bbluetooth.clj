(ns bluetooth
  (:require
   [babashka.cli :as cli]
   [babashka.process :as bp]
   [clojure.string :as str]
   [lib.notifications :as notifications]
   [lib.recent]
   [lib.rofi :as rofi]
   [lib.shell :as shell]))

(def recent-db-name "bbluetooth")

;; Helpers ---------------------------------------------------------------------

(defn get-card-name-for-device
  "Get PulseAudio card name from bluetooth device MAC address."
  [device-id]
  (let [normalized-id (str/replace device-id ":" "_")
        card-name (str "bluez_card." normalized-id)]
    card-name))

(defn get-current-profile
  "Get the current profile for a bluetooth device card."
  [device-id]
  (let [card-name (get-card-name-for-device device-id)]
    (try
      (let [result (bp/sh ["pactl" "list" "cards"])]
        (when (zero? (:exit result))
          (let [output (:out result)
                ;; Find the card section
                card-section (re-find (re-pattern (str "(?s)Name: " (java.util.regex.Pattern/quote card-name) ".*?(?=Name:|\\z)")) output)]
            (when card-section
              ;; Extract active profile
              (when-let [match (re-find #"Active Profile: (.+)" card-section)]
                (second match))))))
      (catch Exception _e nil))))

(defn is-hfp-only?
  "Check if device is connected with only HFP profile."
  [device-id]
  (when-let [profile (get-current-profile device-id)]
    (and (str/includes? (str/lower-case profile) "hfp")
         (not (str/includes? (str/lower-case profile) "a2dp")))))

(defn set-a2dp-profile!
  "Force A2DP profile for a bluetooth device. Tries multiple profile names."
  [device-id]
  (let [card-name (get-card-name-for-device device-id)
        profiles ["a2dp_sink_aac" "a2dp_sink_sbc" "a2dp_sink" "a2dp"]]
    ;; Wait a moment for the device to fully connect
    (Thread/sleep 2000)
    ;; Try each profile until one succeeds
    (some (fn [profile]
            (try
              (let [result (bp/sh ["pactl" "set-card-profile" card-name profile])]
                (when (zero? (:exit result))
                  (notifications/show (str "Switched to A2DP profile: " profile))
                  true))
              (catch Exception _e nil)))
          profiles)))

(defn bose-qc-35-prepare-connection!
  "Some extra settings to ensure high quality bluetooth is available for bose qc35."
  []
  (->> ["power on"
        "discoverable on"
        "pairable on"
        "agent NoInputNoOutput"
        "default-agent"
        "connect 04:52:C7:C6:1B:68"]
       (mapv #(bp/sh ["bluetoothctl" %])))
  (set-a2dp-profile! "04:52:C7:C6:1B:68"))

(defn split-device-info [device-str]
  (let [[_ id name] (re-find #"Device (\S+) (.+)$" device-str)]
    {:id id :name name}))

(defn devices []
  (let [devices (->> (shell/lines "bluetoothctl devices")
                     (map split-device-info))]
    (lib.recent/sort! devices recent-db-name {:key-fn :id})))

(defn bluetooth-enable! [on?]
  (bp/sh ["bluetooth" (if on? "on" "off")]))

(defn device-disconnect!
  "Disconnect a bluetooth device."
  [id]
  (bp/sh ["bluetoothctl" "disconnect" id]))

(defn device-connect-with-retry!
  "Connect device and retry if only HFP is available, until A2DP sink is established."
  [{:keys [id] :as _device} max-retries]
  (loop [attempt 1]
    (let [result (bp/sh ["bluetoothctl" "connect" id])]
      (if-not (zero? (:exit result))
        ;; Connection failed
        result
        ;; Connection succeeded, check profile
        (do
          (set-a2dp-profile! id)
          (Thread/sleep 1000) ; Give it a moment to settle
          (if (is-hfp-only? id)
            (if (< attempt max-retries)
              (do
                (notifications/show (str "Only HFP available, retrying... (attempt " attempt "/" max-retries ")"))
                (device-disconnect! id)
                (Thread/sleep 2000) ; Wait before reconnecting
                (recur (inc attempt)))
              (do
                (notifications/show (str "Failed to get A2DP after " max-retries " attempts"))
                result))
            (do
              (when (> attempt 1)
                (notifications/show (str "A2DP established on attempt " attempt)))
              result)))))))

(defn device-connect! [{:keys [name id] :as device}]
  (bluetooth-enable! true)
  (if (= name "Flo Bose")
    (bose-qc-35-prepare-connection!)
    (do
      (lib.recent/inc-db-entry! recent-db-name id)
      (device-connect-with-retry! device 5))))

(defn rofi-connect! []
  (bluetooth-enable! true)
  (let [{:keys [exit out] :as resp} (-> (rofi/select (devices) {:prompt "BT connect" :to-title :name})
                                        (device-connect!))]
    (when-not (zero? exit)
      (notifications/error out))
    resp))

(comment
  (def err (rofi-connect!))
  nil)

;; Commands --------------------------------------------------------------------

(defn rofi-connect-cmd [_]
  (rofi-connect!))

(defn bluetootho-on-cmd [_]
  (bluetooth-enable! true))

(defn bluetootho-off-cmd [_]
  (bluetooth-enable! false))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["connect"] :fn rofi-connect-cmd :args->opts [:device]}
   {:cmds ["rofi"] :fn rofi-connect-cmd}
   {:cmds ["on"] :fn bluetootho-on-cmd}
   {:cmds ["off"] :fn bluetootho-off-cmd}])

(defn -main [& args]
  (cli/dispatch table args))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

(comment
  (-main)
  nil)
