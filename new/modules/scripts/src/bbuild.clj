(ns bbuild
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [cats.core :as m]
   [cats.monad.exception :as exc]
   [clojure.edn :as edn]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [lib.fp]
   [lib.fs]
   [lib.monad]
   [lib.shell :refer [sh-exc]]
   [lib.xdg :as xdg])
  (:import '(java.util.Date)))

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
   (let [{:keys [command dir]} item]
     (update coll parent (fn [items] (-> (remove (fn [i] (and (= (:command i) command)
                                                              (= (:dir i) dir)))
                                                 items)
                                         (conj (assoc item :id (random-uuid)))))))))

(defn add-items!
  ([items]
   (add-items! items (:parent defaults)))
  ([items parent]
   (let [coll (exc/extract (read-config-file!) {})
         new-coll (reduce (fn [_acc cur] (add-item cur parent coll)) coll items)]
     (save-config-file! new-coll))))

(defn execute-cmd [{:keys [opts]}]
  (let [{:keys [command dir]} opts
        cwd (if (fs/absolute? dir)
              dir
              (lib.fs/current-directory))
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
      (->> output
           (m/return)
           (lib.monad/tap (fn [_]
                            (->
                             {:command command
                              :updated-at (System/currentTimeMillis)}
                             (cond-> dir (assoc :dir dir))
                             (#(add-items! [%] (str git-root))))))))))

(comment
  (execute-cmd {:opts {:command "ls -la"}})
  (execute-cmd {:opts {:command "ls" :dir "new"}})
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


  nil)
