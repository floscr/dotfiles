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

(defn add-bookmark!
  ([item]
   (add-bookmark! item (:parent defaults)))
  ([item parent]
   (->> (exc/extract (read-bookmarks-file!) nil)
        (add-bookmark item parent)
        (save-bookmarks!))))

(defn list-bookmarks
  ([] (list-bookmarks (:parent defaults)))
  ([parent]
   (m/mlet [coll (read-bookmarks-file!)]
     (if-let [coll* (get coll parent)]
       (exc/success coll*)
       (exc/failure (Exception. (format "No such parent found: %s" parent)))))))

;; Commands --------------------------------------------------------------------

(defn list-bookmarks-cmd [{:keys [opts]}]
  (let [{:keys [parent debug with-action]} opts
        items (->> (list-bookmarks (or parent (:parent defaults)))
                   (m/fmap (fn [xs] (map (fn [{:keys [name file commands]
                                               :or {commands []}}]
                                           (let [name (or name file)]
                                             (if with-action
                                               (let [file-action (some->> file
                                                                          fs/expand-home
                                                                          str
                                                                          (conj [:open-file])
                                                                          (vector))
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
        parent (or parent (:parent defaults))
        items (->> input
                   (edn/read-string)
                   (exc/try-on)
                   (m/fmap (fn [x] (if (or (seq? x) (vector? x)) x [x]))))]
    items))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["list"] :args->opts [:parent] :fn list-bookmarks-cmd}
   {:cmds ["list"] :fn list-bookmarks-cmd}
   {:cmds ["add"] :args->opts [:input] :fn add-bookmarks-cmd}])

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

  (b (add-bookmark! {:file "~/.config/doom/modules/private/work/config.org"}))

  nil)
