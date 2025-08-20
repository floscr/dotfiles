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
  (let [git-root (get-git-root execution-dir)]
    (sqlite/execute! db-path
      ["INSERT INTO commands (command, execution_dir, git_root, count, last_executed)
        VALUES (?, ?, ?, 1, CURRENT_TIMESTAMP)
        ON CONFLICT(command, execution_dir) DO UPDATE SET
          count = count + 1,
          last_executed = CURRENT_TIMESTAMP"
       command execution-dir git-root])))

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
    (when command
      (log-command! command execution-dir))))

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

(defn filter-commands [commands query]
  (if (empty? query)
    commands
    (let [query-lower (str/lower-case query)]
      (->> commands
           (filter #(str/includes? (str/lower-case (:command %)) query-lower))
           (sort-by #(str/index-of (str/lower-case (:command %)) query-lower))))))

(defn clear-screen []
  (print "\033[2J\033[H")
  (flush))

(defn move-cursor [row col]
  (print (format "\033[%d;%dH" row col))
  (flush))

(defn hide-cursor []
  (print "\033[?25l")
  (flush))

(defn show-cursor []
  (print "\033[?25h")
  (flush))

(defn draw-prompt [query filtered-commands selected-idx]
  (clear-screen)
  (println (format "Command History > %s" query))
  (println "")
  (let [display-commands (take 10 filtered-commands)]
    (doseq [[idx {:keys [command total_count]}] (map-indexed vector display-commands)]
      (if (= idx selected-idx)
        (println (format "\033[7m> %4d  %s\033[0m" total_count command))
        (println (format "  %4d  %s" total_count command)))))
  (move-cursor 1 (+ 18 (count query))))

(defn read-key []
  (let [k (.read System/in)]
    (if (= k 27) ; ESC sequence
      (let [k2 (.read System/in)]
        (if (= k2 91) ; [
          (let [k3 (.read System/in)]
            (case k3
              65 :up    ; Arrow up
              66 :down  ; Arrow down
              67 :right ; Arrow right  
              68 :left  ; Arrow left
              k3))
          k2))
      k)))

(defn cleanup-terminal []
  (clear-screen)
  (show-cursor)
  (bp/shell "stty icanon echo"))

(defn interactive-cmd [_]
  (init-db!)
  (let [all-commands (get-commands-by-frequency)]
    (bp/shell "stty -icanon -echo")
    (hide-cursor)
    (try
      (loop [query ""
             selected-idx 0]
        (let [filtered-commands (filter-commands all-commands query)
              max-idx (max 0 (dec (min 10 (count filtered-commands))))
              current-idx (min selected-idx max-idx)]
          (draw-prompt query filtered-commands current-idx)
          (let [k (read-key)]
            (cond
              ;; Enter key (13)
              (= k 13)
              (when (and (seq filtered-commands) (<= current-idx (dec (count filtered-commands))))
                (let [selected-command (:command (nth filtered-commands current-idx))]
                  (cleanup-terminal)
                  (println selected-command)))
              
              ;; Escape key or Ctrl+C (3)
              (or (= k 27) (= k 3))
              (cleanup-terminal)
              
              ;; Backspace (127 or 8)
              (or (= k 127) (= k 8))
              (recur (if (empty? query) query (subs query 0 (dec (count query)))) 0)
              
              ;; Arrow up or Ctrl+P (16)
              (or (= k :up) (= k 16))
              (recur query (max 0 (dec current-idx)))
              
              ;; Arrow down or Ctrl+N (14)
              (or (= k :down) (= k 14))
              (recur query (min max-idx (inc current-idx)))
              
              ;; Regular character
              (and (integer? k) (>= k 32) (<= k 126))
              (recur (str query (char k)) 0)
              
              ;; Default - continue loop
              :else
              (recur query current-idx)))))
      (catch Exception e
        (cleanup-terminal)
        (throw e)))))

(defn fzf-cmd [_]
  (init-db!)
  (let [commands (get-commands-by-frequency)]
    (doseq [{:keys [command total_count]} commands]
      (println command))))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["log"] :fn log-cmd :args->opts [:command]}
   {:cmds ["list"] :fn list-cmd}
   {:cmds ["project"] :fn project-cmd}
   {:cmds ["fzf"] :fn fzf-cmd}
   {:cmds ["interactive"] :fn interactive-cmd}])

(defn -main [& args]
  (cli/dispatch table args))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

(comment
  (-main "log" "ls")
  nil)
