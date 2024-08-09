(ns lib.recent
  (:require
   [babashka.fs :as fs]
   [clojure.edn :as edn]
   [lib.xdg :as xdg]
   [lib.time :as lt]))

;; DB Paths --------------------------------------------------------------------

(def db-dir
  "Directory path that contains db files."
  (xdg/cache-path "babashka-recent"))

(defn db-file
  "Path to db with `db-name`."
  [db-name]
  (fs/file db-dir (str db-name ".edn")))

(comment
  (require '[lib.emacs :as emacs])
  (emacs/open-file! db-dir)
  nil)

;; DB Operations ---------------------------------------------------------------

(defn touch-entry
  [{:keys [times _last]
    :or {times 0}}]
  {:times (inc times)
   :last (lt/unix-timestamp)})

(defn get-times-touched [db key]
  (get-in db [key :times] 0))

(defn get-last-touched [db key]
  (get-in db [key :last] 0))

;; DB IO -----------------------------------------------------------------------

(defn read-db! [db-name]
  (try
    (-> (db-file db-name)
        (str)
        (slurp)
        (edn/read-string))
    (catch Exception _ {})))

(defn write-db! [db db-name]
  (let [db-path (db-file db-name)]
    (when-not (fs/exists? db-path)
      (fs/create-dirs (fs/parent db-path)))
    (spit (str db-path) (str db)))
  db)

(defn inc-db-entry! [db-name k]
  (when k
    (-> (read-db! db-name)
        (update k touch-entry)
        (write-db! db-name))))

;; Sorting ---------------------------------------------------------------------

(defn sort-by-db-most-used [db key-fn coll]
  (sort-by #(get-times-touched db (key-fn %)) > coll))

(defn sort-by-db-recent [db key-fn coll]
  (sort-by #(get-last-touched db (key-fn %)) > coll))

(defn sort!
  "Sort sequential data `coll` using the persisted sorting db data from `db-name`.

  When the `coll` is a list of maps use `key-fn` in `opts` to access the sorting key.
  E.g.: Sort the `coll` by the `:id` value.
  (sort! [{:id \"foo\"} {:id \"test\"} {:id \"bar\"}] \"test\" {:key-fn :id})

  Use `sort-fn` for custom sorting.
  (sort! [{:id \"foo\"} {:id \"test\"} {:id \"bar\"}] \"test\" {:sort-fn sort-by-db-recent})"
  ([coll db-name & {:keys [key-fn sort-fn]
                    :or {key-fn identity
                         sort-fn sort-by-db-most-used}
                    :as _opts}]
   (let [db (read-db! db-name)]
     (if (seq db)
       (sort-fn db key-fn coll)
       coll))))

(comment
  (inc-db-entry! "test" "test")
  (inc-db-entry! "test" "bar")

  (sort! ["foo" "test" "bar"] "test")
  (sort! ["foo" "test" "bar"] "test" {:sort-fn sort-by-db-recent})
  ;; => ("test" "bar" "foo")

  (sort! [{:id "foo"} {:id "test"} {:id "bar"}] "test" {:key-fn :id})
  nil)
