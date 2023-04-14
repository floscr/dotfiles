(ns bbookmarks
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [cats.core :as m]
   [cats.monad.exception :as exc]
   [clojure.edn :as edn]
   [lib.xdg :as xdg]))

;; Config ----------------------------------------------------------------------

(def script-name "bbookmarks")

(def config-path (xdg/config-path script-name))

(def bookmarks-file (fs/path config-path "bookmarks.edn"))

(def config {:default-project "main"})

;; Helpers ---------------------------------------------------------------------

(defn read-bookmarks-file! []
  (m/mlet [contents (-> bookmarks-file
                        (str)
                        (slurp)
                        (edn/read-string)
                        (exc/try-on))]
    (m/return contents)))

(defn save-bookmarks! [coll]
  (fs/create-dirs config-path)
  (spit (str bookmarks-file) coll))

(defn add-bookmark
  ([item] (add-bookmark item nil))
  ([item {:keys [parent coll]
          :or {coll {}
               parent (:default-project config)}}]
   (update coll parent (fnil conj []) (assoc item :id (random-uuid)))))

(defn add-bookmark!
  ([item] (add-bookmark! item nil))
  ([item cfg]
   (-> (add-bookmark item (assoc cfg :coll (-> (read-bookmarks-file!)
                                               (exc/extract nil))))
       (save-bookmarks!))))

;; Commands --------------------------------------------------------------------

(defn main [args]
  args)


(defn add! [])


;; Main ------------------------------------------------------------------------

(def table
  [{:cmds [] :fn main}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (read-bookmarks-file!)
  (add-bookmark! {:file "~/.config/doom/modules/private/work/config.org"})
  nil)
