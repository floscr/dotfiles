(ns bluetooth
  (:require
   [babashka.cli :as cli]
   [babashka.process :as bp]
   [lib.rofi :as rofi]
   [lib.shell :as shell]))

;; Helpers ---------------------------------------------------------------------

(defn split-device-info [device-str]
  (let [[_ id name] (re-find #"Device (\S+) (.+)$" device-str)]
    {:id id :name name}))

(defn devices []
  (->> (shell/lines "bluetoothctl devices")
       (map split-device-info)))

(defn bluetooth-enable! [on?]
  (bp/sh ["bluetooth" (if on? "on" "off")]))

(defn rofi-connect! []
  (bluetooth-enable! true)
  (let [{:keys [id]} (rofi/select (devices) {:prompt "BT connect"
                                             :to-title :name})]
    (bp/sh ["bluetoothctl" "connect" id])))

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
  [{:cmds [] :fn rofi-connect-cmd}
   {:cmds ["rofi"] :fn rofi-connect-cmd}
   {:cmds ["on"] :fn bluetootho-on-cmd}
   {:cmds ["off"] :fn bluetootho-off-cmd}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (-main)
  nil)
