(ns org-attach
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [babashka.http-client :as http]
   [babashka.process :as bp]
   [clojure.core.match :refer [match]]
   [clojure.java.io :as io]
   [lambdaisland.uri :as uri]
   [lib.clipboard :as clipboard]
   [lib.fs]
   [lib.shell]
   [lib.web]))

;; Config ----------------------------------------------------------------------

(def attach-dir (lib.fs/expand "~/Documents/Org/.attach/"))
(def clojure-attach-dir (fs/path attach-dir "clojure-attach/"))

;; Helpers ---------------------------------------------------------------------

(defn scheme? [s]
  (some? (:scheme (uri/uri s))))

(defn- match-typed [clip]
  (match clip
           ([:string x] :guard (comp scheme? last)) [:url x]
           ([:string x] :guard (comp fs/regular-file? fs/expand-home last)) (-> x
                                                                                (fs/expand-home)
                                                                                (#(vec [:file (fs/file %)])))
           [:file x] [:file x]
           :else [:error (str "Could not find matching schema for clipboard" \newline clip)]))

(defn- clipboard-content->typed []
  (let [clip (clipboard/content-or-file)]
    (match-typed clip)))

(defn string->typed [str]
  (match-typed [:string str]))

(defn org-attach-temp-file!
 "Returns file for a new temp file used for downloading things "
 [& {:as opts}]
 (fs/file (fs/create-temp-file (merge {:prefix "org-attach"} opts))))

(defn download-url [[_ url]]
  (let [resp (http/get url {:throw false
                            :as :stream})]
    (match [(uri/uri url) resp]
      [{:host "twitter.com"} {:status 200}] (let [dir (fs/create-temp-dir)
                                                  filename "video.mp4"
                                                  output-file (fs/path dir filename)
                                                  nitter-url (lib.web/twitter-url->nitter url)]
                                              (bp/sh {:dir dir} "yt-dlp" nitter-url "-o" filename)
                                              (if (fs/exists? output-file)
                                                [:file output-file]
                                                [:error (str "Could not download file " url " " output-file)]))
      [_ {:status 200 :headers headers :body body}] (let [content-type (get headers "content-type")
                                                          [_ ext] (re-find #".+/(\w+)" content-type)]
                                                      (if (re-find #"^text/html" content-type)
                                                        [:error "Pased url is html"]
                                                        (let [file (org-attach-temp-file! :suffix (str "." ext))]
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

(defn attach-typed [typed]
  (-> (match typed
             [:url _] (-> (download-url typed)
                          (attach-file))
             [:file _] (attach-file typed)
             [:error e] (throw (Exception. (str "Error: " \newline e))))
      (org-link)))

;; Commands --------------------------------------------------------------------

(defn main [{:keys [opts]}]
  (let [{:keys [url yank]} opts
        typed (if url
                (string->typed url)
                (clipboard-content->typed))]
    (cond-> (attach-typed typed)
        yank (doto lib.clipboard/set-clip)
        :else (doto println))))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["help"] :fn (fn [_] (prn "org_attach <url>"))}
   {:cmds [] :fn main :args->opts [:url]}])

(defn -main [& args]
  (cli/dispatch table args))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

(comment
  (-main "https://twitter.com/Rainmaker1973/status/1713574100638065097")
  (-main)
  nil)
