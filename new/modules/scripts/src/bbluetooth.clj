(ns bluetooth
  (:require
   [babashka.cli :as cli]
   [babashka.process :as bp]
   [lib.notifications :as notifications]
   [lib.recent]
   [lib.rofi :as rofi]
   [lib.shell :as shell]))

(def recent-db-name "bbluetooth")

;; Helpers ---------------------------------------------------------------------

(defn get-card-name-for-device
  "Get PulseAudio card name from bluetooth device MAC address."
  [device-id]
  (let [normalized-id (clojure.string/replace device-id ":" "_")
        card-name (str "bluez_card." normalized-id)]
    card-name))

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
                #_(when (zero? (:exit result))
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

(defn device-connect! [{:keys [name id]}]
  (bluetooth-enable! true)
  (if (= name "Flo Bose")
    (bose-qc-35-prepare-connection!)
    (do
      (lib.recent/inc-db-entry! recent-db-name id)
      (let [result (bp/sh ["bluetoothctl" "connect" id])]
        ;; Always try to force A2DP after connection
        (set-a2dp-profile! id)
        result))))

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
