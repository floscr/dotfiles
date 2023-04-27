(ns bbookmarks
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [cats.core :as m]
   [cats.monad.exception :as exc]
   [clojure.core.match :refer [match]]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [lib.xdg :as xdg]))

;; Config ----------------------------------------------------------------------

(def script-name "bbookmarks")

(def config-path (xdg/config-path script-name))

(def bookmarks-file (fs/path config-path (format "%s.edn" script-name)))

(def ^:dynamic defaults {:config-path config-path
                         :parent "main"
                         :bookmarks-file bookmarks-file})

;; Helpers ---------------------------------------------------------------------

(def ^:dynamic read-bookmarks-file!
  (fn []
    (m/mlet [contents (-> (:bookmarks-file defaults)
                          (str)
                          (slurp)
                          (edn/read-string)
                          (exc/try-on))]
      (m/return contents))))

(def ^:dynamic save-bookmarks!
  (fn [coll]
    (let [{:keys [config-path bookmarks-file]} defaults]
      (-> (fs/create-dirs config-path)
          (exc/try-on)
          (m/bind (fn [_] (exc/try-on
                           (spit (str bookmarks-file) coll))))
          (m/bind (fn [_] (exc/success coll)))))))

(defn add-bookmark
  ([item parent coll]
   (update coll parent (fnil conj []) (assoc item :id (random-uuid)))))

(defn add-bookmarks!
  ([items]
   (add-bookmarks! items (:parent defaults)))
  ([items parent]
   (m/mlet [coll (read-bookmarks-file!)
            :let [new-coll (reduce (fn [acc cur] (add-bookmark cur parent coll)) coll items)]]
     (save-bookmarks! new-coll))))

(defn list-bookmarks
  ([] (list-bookmarks (:parent defaults)))
  ([parent]
   (m/mlet [coll (read-bookmarks-file!)]
     (if-let [coll* (get coll parent)]
       (exc/success coll*)
       (exc/failure (Exception. (format "No such parent found: %s" parent)))))))

(defn file->action [file parent {:keys [relative-to]} & {:keys [project-root]}]
  (when file
    (let [file* (if (= relative-to :parent)
                  (fs/path (or project-root parent) file)
                  file)]
      (->> file*
           fs/expand-home
           str
           (conj [:open-file])
           (vector)))))

;; Commands --------------------------------------------------------------------

(defn list-bookmarks-cmd [{:keys [opts]}]
  (let [{:keys [parent debug with-action project-root]} opts
        parent (or parent (:parent defaults))
        items (->> (list-bookmarks parent)
                   (m/fmap (fn [xs] (map (fn [{:keys [name file commands]
                                               :or {commands []}
                                               :as item}]
                                           (let [name (or name file)]
                                             (if with-action
                                               (let [file-action (file->action file parent item :project-root project-root)
                                                     commands (into (or file-action []) commands)]
                                                   [name commands])
                                               name)))
                                         xs))))
        result (if debug
                 items
                 (->> (exc/extract items [])
                      (reduce (fn [acc cur]
                                (if with-action
                                  (str acc (first cur) "\n" (second cur) "\n\n")
                                  (str acc cur "\n"))) "")
                      (str/trim)))]
    (doto result println)))

(defn add-bookmarks-cmd [{:keys [opts]}]
  (let [{:keys [input parent]} opts
        parent (or parent (:parent defaults))]
    (->> input
         (edn/read-string)
         (exc/try-on)
         (m/fmap (fn [x] (if (or (seq? x) (vector? x)) x [x])))
         (m/fmap add-bookmarks!))))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["list"] :args->opts [:parent] :fn list-bookmarks-cmd}
   {:cmds ["list"] :fn list-bookmarks-cmd}
   {:cmds ["add"] :args->opts [:input] :fn add-bookmarks-cmd}
   {:cmds ["remove"] :args->opts [:input] :fn add-bookmarks-cmd}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment

  (defmacro b [& body]
    `(binding [read-bookmarks-file! #(exc/success {})]
       ~@body))

  (add-bookmarks-cmd {:opts {:input "[{:file \"foo\" :name \"bar\"}]"}})

  (list-bookmarks-cmd {})

  (-main "list" "--with-action?")

  (-main "list")
  (-main "list" "foo")

  (b (read-bookmarks-file!))

  (b (list-bookmarks))
  (list-bookmarks-cmd {})
  (list-bookmarks-cmd {:opts {:with-action true :debug true}})

  (match (exc/success {:foo 1})
         {:success {:foo x}} x
         {:failure error} error)

  (b (add-bookmarks! [{:file "findme"}]))

  nil)

;; Convert old bookmarks format
(comment
  (->> (read-bookmarks-file!)
       (m/fmap (fn [edn]
                 (->> (mapv (fn [[k vs]] [k (mapv #(if (:id %) % (assoc % :id (random-uuid))) vs)]) edn)
                      (into {})
                      (save-bookmarks!))))))
