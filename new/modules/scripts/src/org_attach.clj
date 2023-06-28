(ns org-attach
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [babashka.http-client :as http]
   [clojure.core.match :refer [match]]
   [clojure.java.io :as io]
   [lambdaisland.uri :as uri]
   [lib.clipboard :as clipboard]
   [lib.fs]
   [lib.shell]))

;; Config ----------------------------------------------------------------------

(def attach-dir (lib.fs/expand "~/Documents/Org/.attach/"))
(def clojure-attach-dir (fs/path attach-dir "clojure-attach/"))

;; Helpers ---------------------------------------------------------------------

(defn scheme? [s]
  (some? (:scheme (uri/uri s))))

(defn- detect-clipboard-contents []
  (let [clip (clipboard/content-or-file)]
    (match clip
           ([:string x] :guard (comp scheme? last)) [:url x]
           ([:string x] :guard (comp fs/regular-file? fs/expand-home last)) (-> x
                                                                                (fs/expand-home)
                                                                                (#(vec [:file (fs/file %)])))
           [:file x] [:file x]
           :else [:error (str "Could not find matching schema for clipboard" \newline clip)])))

(comment
  (let [file (fs/file (fs/create-temp-file {:prefix "org-attach" :suffix ".mp4"}))]
    ["yt-dlp" (str "\"" "https://twitter.com/fasc1nate/status/1602755629042786304" "\"") "-o" file])

  nil)

(defn download-url [[_ url]]
  (let [resp (http/get url {:throw false
                            :as :stream})]
    (match [(uri/uri url) resp]
           [{:host "twitter.com"} {:status 200}] (let [file (fs/file (fs/create-temp-file {:prefix "org-attach" :suffix ".mp4"}))]
                                                   (lib.shell/sh ["yt-dlp" (str "\"" url "\"") "-o" file])
                                                   [:file file])
           [_ {:status 200 :headers headers :body body}] (let [content-type (get headers "content-type")
                                                               [_ ext] (re-find #".+/(\w+)" content-type)]
                                                           (if (re-find #"^text/html" content-type)
                                                             [:error "Pased url is html"]
                                                             (let [file (fs/file (fs/create-temp-file {:prefix "org-attach" :suffix (str "." ext)}))]
                                                               (io/copy body file)
                                                               [:file file])))
           :else [:error resp])))

(defn md5-filename [file]
  (let [md5 (lib.shell/md5 (str file))
        [_f ext] (fs/split-ext (fs/file-name file))]
    (str md5 "." ext)))

(defn attach-file [file]
  (match file
         [:file f] (let [dest (fs/path clojure-attach-dir (md5-filename f))]
                     (try (fs/copy f dest) (catch java.nio.file.FileAlreadyExistsException _))
                     dest)
         :else {:error (str "Could not attach " file)}))

(defn org-link [file]
  (let [path (fs/relativize clojure-attach-dir file)]
    (format "[[my-attach:%s]]" path)))

(defn attach-clipboard []
  (let [clip (detect-clipboard-contents)]
    (-> (match clip
               [:url _] (-> (download-url clip)
                            (attach-file))
               [:file _] (attach-file clip)
               [:error e] (throw (Exception. (str "Error: " \newline e))))
        (org-link))))

;; Commands --------------------------------------------------------------------

(defn main [{:keys [opts]}]
  (let [{:keys [yank]} opts]
    (cond-> (attach-clipboard)
        yank (doto lib.clipboard/set-clip)
        :else (doto println))))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds [] :fn main}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (main {})
  (-main)
  (-main "sub")
  nil)
