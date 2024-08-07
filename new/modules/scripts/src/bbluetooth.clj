(ns bluetooth
  (:require
   [lib.recent]
   [babashka.cli :as cli]
   [babashka.process :as bp]
   [lib.rofi :as rofi]
   [lib.shell :as shell]))

(def recent-db-name "bbluetooth")

;; Helpers ---------------------------------------------------------------------

(defn bose-qc-35-prepare-connection!
  "Some extra settings to ensure high quality bluetooth is available for bose qc35."
  []
  (->> ["power on"
        "discoverable on"
        "pairable on"
        "agent NoInputNoOutput"
        "default-agent"
        "connect 04 :52:C7:C6:1B:68"]
       (mapv #(bp/sh ["bluetoothctl" %])))
  (bp/sh "pacmd set-card-profile bluez_card.04_52_C7_C6_1B_68 a2dp_sink_aac"))

(defn split-device-info [device-str]
  (let [[_ id name] (re-find #"Device (\S+) (.+)$" device-str)]
    {:id id :name name}))

(defn devices []
  (let [devices (->> (shell/lines "bluetoothctl devices")
                     (map split-device-info))]
    (lib.recent/sort! devices recent-db-name :id)))

(defn bluetooth-enable! [on?]
  (bp/sh ["bluetooth" (if on? "on" "off")]))

(defn device-connect! [{:keys [name id]}]
  (bluetooth-enable! true)
  (when (= name "Flo Bose")
    (bose-qc-35-prepare-connection!))
  (lib.recent/inc-db-entry! recent-db-name id)
  (bp/sh ["bluetoothctl" "connect" id]))

(defn rofi-connect! []
  (bluetooth-enable! true)
  (-> (rofi/select (devices) {:prompt "BT connect" :to-title :name})
      (device-connect!)))

(comment
  (rofi-connect!)
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
