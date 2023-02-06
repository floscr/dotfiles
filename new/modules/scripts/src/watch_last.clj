(ns watch_last
  (:require
   [babashka.cli :as cli]
   [babashka.pods :as pods]
   [babashka.process :as bp]
   [lib.fs :as lfs]))

;; pods ------------------------------------------------------------------------

(pods/load-pod (lfs/expand "~/.config/dotfiles/new/modules/scripts/deps/pod-babashka-go-sqlite3.bin"))
(require '[pod.babashka.go-sqlite3 :as sqlite])

;; Config ----------------------------------------------------------------------

(def db (lfs/expand "~/.config/BraveSoftware/Brave-Browser/Profile 2/History"))

(def tmp-db "/tmp/brave-profile-2-history")

;; Helpers ---------------------------------------------------------------------

(defn query-history! []
  ;; Copy database as it might be locked and in use from current browser
  (bp/sh (format "cp %s %s" (pr-str db) (pr-str tmp-db)))
  (->> (sqlite/query tmp-db "select datetime(last_visit_time/1000000-11644473600,'unixepoch'),url from urls where url like '%watchseries%' order by last_visit_time desc limit 5")
       (map :url)))

;; Commands --------------------------------------------------------------------

(defn reopen-last-watched [_]
  (some-> (query-history!)
          first
          (#(bp/shell (format "xdg-open %s" (pr-str %))))))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds [] :fn reopen-last-watched}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (-main)
  nil)
