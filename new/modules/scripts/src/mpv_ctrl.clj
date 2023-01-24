(ns mpv-ctrl
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [babashka.process :as bp]
   [clojure.edn :as edn]
   [clojure.string :as str]))

(def ^:private log-file
  (-> (fs/path (System/getenv "XDG_CACHE_HOME") "mpv_history_log.edn")
      (str)))

(defn write-log-entry! [entry]
  (let [entries (or (when (fs/exists? log-file)
                      (edn/read-string (slurp log-file)))
                    [])
        new-entries (conj entries entry)]
    (when (= last entries entry)
      (spit log-file new-entries))))

(defn- log-items []
  (when (fs/exists? log-file)
    (->> (slurp log-file)
         (edn/read-string))))

(defn safe-parse-int [val]
  (try (Integer/parseInt val)
     (catch Exception _ nil)))

;; Commands --------------------------------------------------------------------

(defn help! [_]
  (println "Help"))

(defn add-log! [{:keys [opts]}]
  (let [{:keys [event path title]} opts
        entry {:event event
               :path path
               :title title}]
    (when (= event "loaded")
      (write-log-entry! entry)
      (println "Added entry to log file."))))

(defn list-log! [_]
  (for [{:keys [title]} (log-items)]
    (println title)))

(defn rofi-history! []
  (let [items (log-items)
        rofi-lines (->> items
                        (map #(:title %))
                        (str/join "\n"))]
    (some-> (bp/sh {:in rofi-lines
                    :continue true}
                   "rofi -i -levenshtein-sort -dmenu -p \"Play\" -format i")
            :out
            (str/trim)
            (safe-parse-int)
            (#(nth items %))
            :path
            (#(bp/process "mpv" % "&")))))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["add-log"] :args->opts [:event :path :title] :fn add-log!}
   {:cmds ["list-log"] :fn list-log!}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (-main "list-log")
  nil)
