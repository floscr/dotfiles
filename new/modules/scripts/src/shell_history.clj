(ns shell-history
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [babashka.pods :as pods]
   [babashka.process :as bp]
   [clojure.string :as str]
   [honeysql.core :as sql]))

;; Pods ------------------------------------------------------------------------

(pods/load-pod "pod-babashka-go-sqlite3")

(require '[pod.babashka.go-sqlite3 :as sqlite])

;; Database --------------------------------------------------------------------

(def db-path (str (fs/path (System/getenv "XDG_DATA_HOME") "shell-history.db")))

(defn init-db! []
  (sqlite/execute! db-path
    ["CREATE TABLE IF NOT EXISTS commands (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       command TEXT NOT NULL,
       execution_dir TEXT NOT NULL,
       git_root TEXT,
       count INTEGER DEFAULT 1,
       first_executed DATETIME DEFAULT CURRENT_TIMESTAMP,
       last_executed DATETIME DEFAULT CURRENT_TIMESTAMP,
       UNIQUE(command, execution_dir)
     )"]))

(defn get-git-root [dir]
  (try
    (let [result (bp/shell {:dir dir :out :string} "git rev-parse --show-toplevel")]
      (when (zero? (:exit result))
        (str/trim (:out result))))
    (catch Exception _ nil)))

(defn log-command! [command execution-dir]
  (let [git-root (get-git-root execution-dir)
        upsert-sql (sql/format
                    {:insert-into :commands
                     :columns [:command :execution_dir :git_root :count :last_executed]
                     :values [[command execution-dir git-root 1 "CURRENT_TIMESTAMP"]]
                     :on-conflict [:command :execution_dir]
                     :do-update-set {:count "count + 1"
                                     :last_executed "CURRENT_TIMESTAMP"}})]
    (sqlite/execute! db-path upsert-sql)))

(defn get-commands-by-frequency []
  (let [query (sql/format
               {:select [:command
                         [:%sum.count :total_count]
                         [:%group_concat.execution_dir :dirs]]
                :from [:commands]
                :group-by [:command]
                :order-by [[:total_count :desc]]})]
    (sqlite/query db-path query)))

(defn get-commands-for-project [git-root]
  (let [query (sql/format
               {:select [:command :execution_dir :count :last_executed]
                :from [:commands]
                :where [:= :git_root git-root]
                :order-by [[:last_executed :desc]]})]
    (sqlite/query db-path query)))

(defn get-commands-for-current-project []
  (let [current-dir (System/getProperty "user.dir")
        git-root (get-git-root current-dir)]
    (if git-root
      (get-commands-for-project git-root)
      [])))

;; CLI -------------------------------------------------------------------------

(defn log-cmd [{:keys [opts args]}]
  (init-db!)
  (let [command (or (:command opts) (first args))
        execution-dir (or (:execution-dir opts) (System/getProperty "user.dir"))]
    (if command
      (do
        (log-command! command execution-dir)
        (println "Command logged:" command))
      (println "Usage: shell_history log <command>"))))

(defn list-cmd [_]
  (init-db!)
  (let [commands (get-commands-by-frequency)]
    (doseq [{:keys [command total_count dirs]} commands]
      (println (format "%4d  %s" total_count command))
      (when (> (count (str/split dirs #",")) 1)
        (println (format "      (used in: %s)" dirs))))))

(defn project-cmd [_]
  (init-db!)
  (let [commands (get-commands-for-current-project)]
    (if (empty? commands)
      (println "No commands found for current project (not in a git repository?)")
      (doseq [{:keys [command execution_dir count last_executed]} commands]
        (println (format "%4d  %s" count command))
        (println (format "      %s (%s)" execution_dir last_executed))))))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["log"] :fn log-cmd :args->opts [:command]}
   {:cmds ["list"] :fn list-cmd}
   {:cmds ["project"] :fn project-cmd}])

(defn -main [& args]
  (cli/dispatch table args))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

(comment
  (-main "log" "ls")
  nil)
