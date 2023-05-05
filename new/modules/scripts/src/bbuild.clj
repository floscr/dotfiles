(ns bbuild
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [babashka.process :as process]
   [cats.core :as m]
   [cats.monad.exception :as exc]
   [cats.monad.maybe :as maybe]
   [clojure.edn :as edn]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   lib.fs
   lib.monad
   [lib.shell :refer [sh-exc]]
   [lib.xdg :as xdg]))

;; Config ----------------------------------------------------------------------

(def script-name (str *ns*))

(def config-path (xdg/config-path script-name))

(def config-file (fs/path config-path (format "%s.edn" script-name)))

(def ^:dynamic defaults {:config-path config-path
                         :config-file config-file})

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

(defn add-item
  ([item parent coll]
   (update coll parent (fnil conj []) (assoc item :id (random-uuid)))))

;; (defn add-items!
;;   ([items]
;;    (add-items! items (:parent defaults)))
;;   ([items parent]
;;    (m/mlet [coll (read-config-file!)]
;;      (let [new-coll (reduce (fn [_acc cur] (add-item cur parent coll)) coll items)]
;;        (save-config-file! new-coll)))))

;; (defn list-items
;;   ([] (list-items (:parent defaults)))
;;   ([parent]
;;    (m/mlet [coll (read-config-file!)]
;;      (if-let [coll* (get coll parent)]
;;        (exc/success coll*)
;;        (exc/failure (Exception. (format "No such parent found: %s" parent)))))))

;; (defn remove-with-id [id coll]
;;   (->> coll
;;        (eduction (map (fn [[k v]] [k (remove #(= (:id %) id) v)]))
;;                  (filter (fn [[_ v]] (seq v))))
;;        (into (hash-map))))

;; ;; Commands --------------------------------------------------------------------

;; (defn list-items-cmd [{:keys [opts]}]
;;   (let [{:keys [parent debug with-action project-root]} opts
;;         parent (or parent (:parent defaults))
;;         items (->> (list-items parent)
;;                    (m/fmap (fn [xs] (map (fn [{:keys [name id pwd command]
;;                                                :or {commands []}
;;                                                :as item}]
;;                                            (let [name (or name command)]
;;                                              (if with-action
;;                                                (let [file-action (file->action file parent item :project-root project-root)
;;                                                      commands (into (or file-action []) commands)]
;;                                                    [name id commands])
;;                                                name)))
;;                                          xs))))
;;         result (if debug
;;                  items
;;                  (->> (exc/extract items [])
;;                       (reduce (fn [acc cur]
;;                                 (if with-action
;;                                   (str acc (str/join "\n" cur) "\n\n")
;;                                   (str acc cur "\n"))) "")
;;                       (str/trim)))]
;;     (doto result println)))

;; (defn add-item-cmd [{:keys [opts]}]
;;   (let [{:keys [input parent]} opts
;;         parent (or parent (:parent defaults))]
;;     (m/mlet [conf])
;;     (->> input
;;          (edn/read-string)
;;          (exc/try-on)
;;          (m/fmap (fn [x] (if (or (seq? x) (vector? x)) x [x])))
;;          (m/fmap (fn [xs] (add-items! xs parent))))))

;; (defn remove-item-cmd [{:keys [opts]}]
;;   (let [{:keys [id]} opts]
;;     (m/mlet [id (exc/try-on (parse-uuid id))
;;              coll (read-config-file!)]
;;       (->> (exc/try-on (remove-with-id id coll))
;;            (m/fmap save-config-file!)))))

(defn git-root []
  (sh-exc "git rev-parse --show-toplevel"))

(type {})
(array-map)

(defn git-worktree-root [worktree-name]
  (let [output (process/sh ["git" "worktree" "list"] {:dir "/home/floscr/Code/Work/Pitch/pitch-app/.worktrees/florian"})]
    (some #(when (re-find (str "^" %1 "\\s" worktree-name) %2)
             (-> %2
                 (str/split #" ")
                 first))
          (repeat #"^(.+)\s(.+)\s\[(.+)\]$")
          (str/split-lines output))))

(fs/path (lib.fs/current-directory) nil)

(m/mlet [foo (maybe/nothing)]
  foo)

(defn execute-cmd [{:keys [opts]}]
  (let [{:keys [command dir]} opts
        cwd (lib.fs/current-directory)
        not-in-git-repo-err (format "Not in a git directory: %s" cwd)]
    (m/mlet [git-root (-> (lib.fs/find-git-root cwd)
                          (lib.monad/some-try not-in-git-repo-err))
             worktree-or-git-root (-> (lib.fs/find-git-worktree-root cwd)
                                      (lib.monad/some-try not-in-git-repo-err))
             execution-dir (exc/try-on
                            (cond
                              (nil? dir) worktree-or-git-root
                              (fs/relative? dir) (fs/path worktree-or-git-root dir)
                              (fs/absolute? dir) dir))
             output (sh-exc command {:dir (str execution-dir)})]
      (-> output
          (str/trim)
          (str/split-lines)))))

(m/pure (exc/success 1))

(comment
  (execute-cmd {:opts {:command "ls"}})
  (execute-cmd {:opts {:command "ls" :dir "new"}})
  (execute-cmd {:opts {:command "ls" :dir "/tmp"}})
  nil)

;; Main ------------------------------------------------------------------------

(def table
  [#_{:cmds ["history"] :fn list-items-cmd}
   {:cmds ["execute"] :args->opts [:command] :fn execute-cmd}])
   ;; {:cmds ["remove"] :args->opts [:id] :fn remove-item-cmd}


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

  (b (add-items! [{:file "findme"}]))
  (b (add-items! [{:file "findme"}] "foo"))

  nil)
