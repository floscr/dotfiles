(ns watch_last
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [babashka.pods :as pods]
   [babashka.process :as bp]
   [clojure.string :as str]
   [lib.shell :refer [sh-lines]]))

;; pods ------------------------------------------------------------------------

(pods/load-pod (-> (fs/expand-home "~/.config/dotfiles/new/modules/scripts/deps/pod-babashka-go-sqlite3.bin") (str)))
(require '[pod.babashka.go-sqlite3 :as sqlite])

;; Config ----------------------------------------------------------------------

(def db (-> "~/.config/BraveSoftware/Brave-Browser/Profile 2/History"
            fs/expand-home
            str))

(def tmp-db "/tmp/brave-profile-2-history")


;; Helpers ---------------------------------------------------------------------

(defn query-history! []
  ;; Copy database as it might be locked and in use from current browser
  (bp/sh (format "cp \"%s\" %s" db tmp-db))
  (->> (sqlite/query tmp-db "select datetime(last_visit_time/1000000-11644473600,'unixepoch'),url from urls where url like '%watchseries%' order by last_visit_time desc limit 5")
       (map :url)))

;; Commands --------------------------------------------------------------------

(defn reopen-last-watched [_]
  (some-> (query-history!)
          first
          (#(bp/shell (format "xdg-open \"%s\"" %)))))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds [] :fn reopen-last-watched}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (-main)
  nil)
