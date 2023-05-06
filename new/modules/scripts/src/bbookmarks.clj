(ns bbookmarks
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [cats.core :as m]
   [cats.monad.exception :as exc]
   [clojure.edn :as edn]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [lib.xdg :as xdg]))

;; Config ----------------------------------------------------------------------

(def script-name (str *ns*))

(def config-path (xdg/config-path script-name))

(def config-file (fs/path config-path (format "%s.edn" script-name)))

(def ^:dynamic defaults {:config-path config-path
                         :config-file config-file
                         :parent "main"})

;; Helpers ---------------------------------------------------------------------

(def ^:dynamic read-config-file!
  (fn []
    (m/mlet [contents (-> (:config-file defaults)
                          (str)
                          (slurp)
                          (edn/read-string)
                          (exc/try-on))]
      (m/return contents))))

(def ^:dynamic save-config-file!
  (fn [coll]
    (let [{:keys [config-path config-file]} defaults]
      (-> (fs/create-dirs config-path)
          (exc/try-on)
          (m/bind (fn [_] (exc/try-on
                           (spit (str config-file)
                                 (with-out-str (pprint/pprint coll))))))
          (m/bind (fn [_] (exc/success coll)))))))

(defn add-bookmark
  ([item parent coll]
   (update coll parent (fnil conj []) (assoc item :id (random-uuid)))))

(defn add-bookmarks!
  ([items]
   (add-bookmarks! items (:parent defaults)))
  ([items parent]
   (m/mlet [coll (read-config-file!)]
     (let [new-coll (reduce (fn [acc cur] (add-bookmark cur parent acc)) coll items)]
       (save-config-file! new-coll)))))

(defn list-bookmarks
  ([] (list-bookmarks (:parent defaults)))
  ([parent]
   (m/mlet [coll (read-config-file!)]
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

(defn remove-with-id [id coll]
  (->> coll
       (eduction (map (fn [[k v]] [k (into [] (remove #(= (:id %) id) v))]))
                 (filter (fn [[_ v]] (seq v))))
       (into (hash-map))))

;; Commands --------------------------------------------------------------------

(defn list-bookmarks-cmd [{:keys [opts]}]
  (let [{:keys [parent debug with-action project-root]} opts
        parent (or parent (:parent defaults))
        items (->> (list-bookmarks parent)
                   (m/fmap (fn [xs] (map (fn [{:keys [name id file commands]
                                               :or {commands []}
                                               :as item}]
                                           (let [name (or name file)]
                                             (if with-action
                                               (let [file-action (file->action file parent item :project-root project-root)
                                                     commands (into (or file-action []) commands)]
                                                   [name id commands])
                                               name)))
                                         xs))))
        result (if debug
                 items
                 (->> (exc/extract items [])
                      (reduce (fn [acc cur]
                                (if with-action
                                  (str acc (str/join "\n" cur) "\n\n")
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
         (m/fmap (fn [xs] (add-bookmarks! xs parent))))))

(defn remove-bookmarks-cmd [{:keys [opts]}]
  (let [{:keys [id]} opts]
    (m/mlet [id (exc/try-on (parse-uuid id))
             coll (read-config-file!)]
      (->> (exc/try-on (remove-with-id id coll))
           (m/fmap save-config-file!)))))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["list"] :args->opts [:parent] :fn list-bookmarks-cmd}
   {:cmds ["list"] :fn list-bookmarks-cmd}
   {:cmds ["add"] :args->opts [:input] :fn add-bookmarks-cmd}
   {:cmds ["remove"] :args->opts [:id] :fn remove-bookmarks-cmd}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (defonce state (atom {}))
  (reset! state {})
  (defmacro b [& body]
    `(binding [read-config-file! #(exc/success @state)
               save-config-file! (fn [x] (reset! state x) (exc/success x))]
       ~@body))

  (remove-with-id (-> @state (get "main") first :id) (assoc @state "foo" []))

  (b (add-bookmarks! [{:file "findme"}]))
  (b (add-bookmarks! [{:file "findme"}] "foo"))

  nil)
