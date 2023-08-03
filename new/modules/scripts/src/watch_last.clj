(ns watch_last
  (:require
   [babashka.cli :as cli]
   [babashka.pods :as pods]
   [babashka.process :as bp]
   [lib.fs :as lfs]
   [lib.rofi :as rofi]))

;; pods ------------------------------------------------------------------------

(pods/load-pod "pod-babashka-go-sqlite3")
(require '[pod.babashka.go-sqlite3 :as sqlite])

;; Config ----------------------------------------------------------------------

(def db (lib.fs/expand "~/.config/BraveSoftware/Brave-Browser/Profile 2/History"))

(def tmp-db "/tmp/brave-profile-2-history")

;; Helpers ---------------------------------------------------------------------

(defn query-history! []
  ;; Copy database as it might be locked and in use from current browser
  (bp/sh (format "cp %s %s" (pr-str db) (pr-str tmp-db)))
  (->> (sqlite/query tmp-db "select datetime(last_visit_time/1000000-11644473600,'unixepoch'),url from urls where url like '%watchseries%' order by last_visit_time desc limit 200")
       (map :url)))

;; Commands --------------------------------------------------------------------

(defn reopen-last-watched-cmd [_]
  (some-> (query-history!)
          first
          (#(bp/shell (format "xdg-open %s" (pr-str %))))))

(defn rofi-history-cmd
  ([] (rofi-history-cmd {}))
  ([_]
   (some-> (query-history!)
           (rofi/select)
           (#(bp/shell (format "xdg-open %s" (pr-str %)))))))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["rofi"] :fn rofi-history-cmd}
   {:cmds [] :fn reopen-last-watched-cmd}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (-main)
  (rofi-history-cmd)
  nil)
