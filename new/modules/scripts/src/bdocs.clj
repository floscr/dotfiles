(ns bdocs
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [bscan]
   [cats.monad.exception :as exception]
   [lib.fs]
   [lib.shell]
   [lib.web]
   org-attach))

;; Config ----------------------------------------------------------------------

(def attach-dir (lib.fs/expand "~/Code/Projects/bbeancount/.attach/"))

;; Main ------------------------------------------------------------------------

(defn scan- [opts]
  (let [out-file (fs/create-temp-file {:prefix "bscan-" :suffix ".pdf"})
        opts (assoc-in opts [:opts :out] out-file)]
    (when (exception/success? (bscan/main opts))
      (cond-> (org-attach/attach-typed [:file out-file] {:attach-dir attach-dir})
        :else (doto println)))))

(defn main [opts]
  (if (get-in opts [:opts :out-file])
    (org-attach/main (assoc-in opts [:opts :attach-dir] attach-dir))
    (main opts)))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds [] :fn main :args->opts [:out-file]}])

(defn -main [& args]
  (cli/dispatch table args))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
