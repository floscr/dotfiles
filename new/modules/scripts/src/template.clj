(ns template
  (:require
   [babashka.cli :as cli]
   [babashka.process :as bp]
   [clojure.string :as str]
   [lib.num :as num]
   [lib.shell :as shell]))

;; Config ----------------------------------------------------------------------

;; Helpers ---------------------------------------------------------------------

;; Commands --------------------------------------------------------------------

(defn main [args]
  args)

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds [] :fn main}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (-main)
  (-main "sub")
  nil)
