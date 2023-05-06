#!/usr/bin/env bb
(ns bbuild
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [cats.core :as m]
   [cats.monad.exception :as exc]
   [cats.monad.maybe :as maybe]
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

(defn try-read-edn [path]
  (m/mlet [contents (exc/try-on (->> path
                                     (str)
                                     (slurp)
                                     (edn/read-string)))]
    (m/return contents)))

(def ^:dynamic read-config-file!
  (fn []
    (try-read-edn (:config-file defaults))))

(def ^:dynamic save-config-file!
  (fn [coll]
    (let [{:keys [config-path config-file]} defaults]
      (-> (fs/create-dirs config-path)
          (exc/try-on)
          (m/bind (fn [_] (exc/try-on
                           (spit (str config-file)
                                 (with-out-str (pprint/pprint coll))))))
          (m/bind (fn [_] (exc/success coll)))))))

(def ^:dynamic read-project-config-file!
  (fn [root]
    (-> (fs/path root (fs/file-name config-file))
        (try-read-edn))))

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
        cwd (if (some-> dir (fs/absolute?))
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
                              (fs/absolute? dir) (str worktree-or-git-root)))
             output (sh-exc (str/join " " command) {:dir (str execution-dir)})]
      (->> output
           (m/return)
           (lib.monad/tap println)
           (lib.monad/tap (fn [_]
                            (->
                             {:command (str/join " " command)
                              :updated-at (System/currentTimeMillis)}
                             (cond-> dir (assoc :dir (str execution-dir)))
                             (#(add-items! [%] (str git-root))))))))))

(defn list-items [dir]
  (m/mlet [coll (read-config-file!)]
    (if-let [coll* (get coll (str dir))]
      (exc/success coll*)
      (exc/failure (Exception. (format "No such dir found: %s" dir))))))

(defn list-items-cmd [{:keys [opts]}]
  (let [{:keys [dir debug with-action]} opts
        parent (lib.fs/find-git-root (or dir (lib.fs/current-directory)))
        items (->> (list-items parent)
                   (m/fmap (fn [xs] (map (fn [{:keys [command id]}]
                                           (if with-action
                                             [command id]
                                             command))
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

(comment
  (list-items-cmd {:opts {:dir "/home/floscr/Code/Projects/org-web/projects/backend/src/com/org_web/feat/org/agenda/core.clj"}})
  (execute-cmd {:opts {:command "ls -la"}})
  (execute-cmd {:opts {:command "ls" :dir "new"}})
  nil)

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["list"] :fn list-items-cmd}
   {:cmds ["execute"]
    :coerce {:command []} :args->opts (repeat :command)
    :fn execute-cmd}])
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
