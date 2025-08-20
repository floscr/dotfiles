(ns shell-history
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [babashka.pods :as pods]
   [babashka.process :as bp]
   [clojure.string :as str]))

;; Pods ------------------------------------------------------------------------

(pods/load-pod "pod-babashka-go-sqlite3")

(require '[pod.babashka.go-sqlite3 :as sqlite])

;; Database --------------------------------------------------------------------

(def db-path (str (fs/path (System/getenv "XDG_DATA_HOME") "shell-history.db")))

(defn init-db! []
  (sqlite/execute! db-path
    "CREATE TABLE IF NOT EXISTS commands (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       command TEXT NOT NULL,
       execution_dir TEXT NOT NULL,
       git_root TEXT,
       count INTEGER DEFAULT 1,
       first_executed DATETIME DEFAULT CURRENT_TIMESTAMP,
       last_executed DATETIME DEFAULT CURRENT_TIMESTAMP,
       UNIQUE(command, execution_dir)
     )"))

(defn get-git-root [dir]
  (try
    (let [result (bp/shell {:dir dir :out :string} "git rev-parse --show-toplevel")]
      (when (zero? (:exit result))
        (str/trim (:out result))))
    (catch Exception _ nil)))

(defn log-command! [command execution-dir]
  (let [git-root (get-git-root execution-dir)]
    (sqlite/execute! db-path
      "INSERT INTO commands (command, execution_dir, git_root, count, last_executed)
       VALUES (?, ?, ?, 1, CURRENT_TIMESTAMP)
       ON CONFLICT(command, execution_dir) DO UPDATE SET
         count = count + 1,
         last_executed = CURRENT_TIMESTAMP"
      [command execution-dir git-root])))

(defn get-commands-by-frequency []
  (sqlite/query db-path
    "SELECT command, SUM(count) as total_count, 
            GROUP_CONCAT(DISTINCT execution_dir) as dirs
     FROM commands 
     GROUP BY command 
     ORDER BY total_count DESC"))

(defn get-commands-for-project [git-root]
  (sqlite/query db-path
    "SELECT command, execution_dir, count, last_executed
     FROM commands 
     WHERE git_root = ? 
     ORDER BY last_executed DESC"
    [git-root]))

(defn get-commands-for-current-project []
  (let [current-dir (System/getProperty "user.dir")
        git-root (get-git-root current-dir)]
    (if git-root
      (get-commands-for-project git-root)
      [])))

;; CLI -------------------------------------------------------------------------

(def cli-spec
  {:log {:desc "Log a command"
         :args->opts [:command]
         :exec-args {:execution-dir (System/getProperty "user.dir")}}
   :list {:desc "List commands by frequency"}
   :project {:desc "List commands for current git project"}})

(defn log-cmd [{:keys [command execution-dir]}]
  (init-db!)
  (log-command! command execution-dir)
  (println "Command logged:" command))

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

(defn -main [& args]
  (cli/dispatch
    {:cmds cli-spec
     :exec-args {:log log-cmd
                 :list list-cmd
                 :project project-cmd}}
    args))

(apply -main *command-line-args*)
