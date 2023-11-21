(ns bdocs
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [bscan]
   [cats.monad.exception :as exception]
   [lib.fs]
   [lib.org :as org]
   [lib.shell]
   [lib.web]
   [org-attach]))

;; Config ----------------------------------------------------------------------

(def attach-file (lib.fs/expand "~/Code/Projects/bbeancount/ressource/documents.org"))
(def attach-dir (lib.fs/expand "~/Code/Projects/bbeancount/.attach/"))

;; Helpers ---------------------------------------------------------------------

(defn org-document-headline [heading-str properties]
  (let [ts (org/now-timestamp->str {:type :inactive})]
    (str "** " heading-str "\n"
         (org/properties->str
          (merge properties
                 {:DATE_CREATED ts
                  :ORIGINAL_DATE ts})) "\n")))

;; Main ------------------------------------------------------------------------

(defn main [opts]
  (let [file (get-in opts [:opts :out-file])
        digital? (not (get-in opts [:opts :scan?]))
        org-link (org-attach/main (-> opts
                                      (assoc-in [:opts :attach-dir] attach-dir)
                                      (assoc-in [:opts :url] file)))
        org-text (org-document-headline org-link (cond-> {}
                                                   (not digital?) (assoc :SCANNED "t")))]
    (fs/write-lines attach-file [org-text] {:append true})))
      
(defn scan- [opts]
  (let [out-file (fs/create-temp-file {:prefix "bscan-" :suffix ".pdf"})
        opts (assoc-in opts [:opts :out] out-file)]
    (when (exception/success? (bscan/main opts))
      (main (-> opts
                (assoc-in [:opts :out-file] out-file)
                (assoc-in [:opts :scan?] true))))))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["scan"] :fn scan-}
   {:cmds [] :fn main :args->opts [:out-file]}])

(defn -main [& args]
  (cli/dispatch table args))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
