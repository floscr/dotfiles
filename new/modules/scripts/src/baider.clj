(ns baider
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]))

(defn parse-patches [patch-string]
  (let [entries (->> patch-string
                     (re-seq #"(.+)\n```.*?
<<<<<<< SEARCH
((?s).*?)=======
((?s).*?)>>>>>>> REPLACE
```\n")
                     (map (fn [[_ filename search-src replace-src]]
                            {:filename filename
                             :search-src search-src
                             :replace-src replace-src})))]
    entries))

(comment
  (->> (slurp "/home/floscr/Code/Work/Hyma/penpot/repo/prompt")
       (parse-patches)
       (map (fn [{:keys [filename search-src replace-src] :as block}]
              (try
                (let [path (str (fs/path "/home/floscr/Code/Work/Hyma/penpot/repo" filename))
                      source (slurp path)
                      modified (str/replace-first source search-src replace-src)]
                  (spit path modified)
                  :done)
                (catch Exception _ block))))
       (remove #(= :done %)))

  nil)
