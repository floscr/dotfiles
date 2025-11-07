#!/usr/bin/env bb
(ns bbluetooth
  (:require
   [babashka.cli :as cli]
   [babashka.process :as bp]
   [lib.notifications :as notifications]
   [lib.recent]
   [lib.rofi :as rofi]
   [lib.shell :as shell]
   [cuerdas.core :as str]))

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
    (and (str/includes? (str/lower profile) "hfp")
         (not (str/includes? (str/lower profile) "a2dp")))))

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

(defn bluetoothctl-info [device-id]
  (bp/sh (str "bluetoothctl info " device-id)))

(defn bluetoothctl-devices []
  (shell/lines "bluetoothctl devices"))

(into #{} (map (comp :name split-device-info) (bluetoothctl-devices)))

(defn devices [& {:keys [keep-up?]}]
  (let [devices (bluetoothctl-devices)
        devices (into #{} (map split-device-info devices))]
    (lib.recent/sort! devices recent-db-name {:key-fn :id})))

(defn available-devices
  "Get all currently available (powered on and in range) devices."
  []
  (let [all-devices (devices)]
    (filter
     (fn [device]
       (try
         (let [info (bluetoothctl-info (:id device))]
           ;; A device is available if it's showing up with valid info
           ;; and not showing "Device not available" error
           (and info
                (str/includes? info "Device")
                (not (str/includes? info "not available"))))
         (catch Exception _e false)))
     all-devices)))

(defn device-connected?
  "Check if a device is currently connected."
  [device-id]
  (try
    (let [info (bp/shell (str "bluetoothctl info " device-id))]
      (and info (str/includes? info "Connected: yes")))
    (catch Exception _e false)))

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

(defn auto-connect-cmd
  "Auto-connect to all available known devices."
  [_]
  (println "Enabling Bluetooth...")
  (bluetooth-enable! true)
  (Thread/sleep 1000) ;; Give Bluetooth a moment to power on

  (println "Scanning for available devices...")
  (let [all-devices (devices)
        connected-count (atom 0)
        failed-devices (atom [])]

    (println (str "Found " (count all-devices) " known device(s)"))

    (doseq [device all-devices]
      (println (str "Checking " (:name device) " (" (:id device) ")..."))

      ;; Check if already connected
      (if (device-connected? (:id device))
        (println (str "  ✓ Already connected to " (:name device)))

        ;; Try to connect
        (try
          (let [result (device-connect! device)]
            (if (zero? (:exit result))
              (do
                (println (str "  ✓ Connected to " (:name device)))
                (swap! connected-count inc))
              (do
                (println (str "  ✗ Failed to connect to " (:name device)))
                (swap! failed-devices conj (:name device)))))
          (catch Exception e
            (println (str "  ✗ Error connecting to " (:name device) ": " (.getMessage e)))
            (swap! failed-devices conj (:name device))))))

    (println)
    (println (str "Auto-connect complete: " @connected-count " device(s) connected"))

    (when (seq @failed-devices)
      (println (str "Failed to connect to: " (str/join ", " @failed-devices))))

    (notifications/show (str "Auto-connect complete: " @connected-count " device(s) connected"))))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["connect"] :fn rofi-connect-cmd :args->opts [:device]}
   {:cmds ["rofi"] :fn rofi-connect-cmd}
   {:cmds ["on"] :fn bluetootho-on-cmd}
   {:cmds ["off"] :fn bluetootho-off-cmd}
   {:cmds ["auto-connect"] :fn auto-connect-cmd}])

(defn -main [& args]
  (cli/dispatch table args))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

(comment
  (-main)
  nil)
