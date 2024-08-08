(ns lib.recent
  (:require
   [babashka.fs :as fs]
   [clojure.edn :as edn]
   [lib.xdg]))

(def cache-path
  (lib.xdg/cache-path "babashka-recent"))

(defn cache-file [file-name]
  (fs/file cache-path (str file-name ".edn")))

(defn read-db! [db-name]
  (try
    (-> (cache-file db-name)
        (str)
        (slurp)
        (edn/read-string))
    (catch Exception _ {})))

(defn write-db! [db db-name]
  (let [db-path (cache-file db-name)]
    (when-not (fs/exists? db-path)
      (fs/create-dirs (fs/parent db-path)))
    (spit (str db-path) (str db)))
  db)

(defn inc-db-entry! [db-name k]
  (-> (read-db! db-name)
      (update k (fnil inc 0))
      (write-db! db-name)))

(defn sort-by-db-most-used [db key-fn coll]
  (sort-by #(get db (key-fn %) 0) > coll))

(defn sort!
  ([coll db-name]
   (sort! coll db-name identity))
  ([coll db-name key-fn]
   (let [db (read-db! db-name)]
     (if (seq db)
       (sort-by-db-most-used db key-fn coll)
       coll))))

(comment
  (inc-db-entry! "test" "test")
  (inc-db-entry! "test" "bar")

  (do
    (require 'lib.emacs)
    (lib.emacs/open-file! (cache-file "test")))

  (sort! ["foo" "test" "bar"] "test")

  (sort! [{:id "foo"} {:id "test"} {:id "bar"}] "test" :id)
  nil)
