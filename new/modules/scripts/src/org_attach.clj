(ns org-attach
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [babashka.http-client :as http]
   [babashka.process :as bp]
   [bscan]
   [clojure.core.match :refer [match]]
   [clojure.java.io :as io]
   [clojure.string :as str]
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

;; TODO Add case for files not found
;; Add better error handling
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

(defn download-url-file! [{:keys [headers body]}]
  (let [content-type (get headers "content-type")
        [_ ext] (re-find #".+/(\w+)" content-type)]
    (if (re-find #"^text/html" content-type)
      [:error "Pased url is html"]
      (let [file (org-attach-temp-file! :suffix (str "." ext))]
        (io/copy body file)
        [:file file]))))

(defn download-twitter! [url]
  (let [dir (fs/create-temp-dir)
        filename "video.mp4"
        output-file (fs/path dir filename)
        nitter-url (lib.web/twitter-url->nitter url)]
    (bp/sh {:dir dir} "yt-dlp" nitter-url "-o" filename)
    (if (fs/exists? output-file)
      [:file output-file]
      [:error (str "Could not download file " url " " output-file)])))

(defn download-url [[_ url]]
  (let [resp (http/get url {:throw false
                            :as :stream})]
    (match [(uri/uri url) resp]
           [{:host "x.com"} _] (download-twitter! url)
      [_ {:status 200 :headers headers :body body}] (download-url-file! {:headers headers
                                                                         :body body})
      :else [:error resp])))

(defn md5-filename [file]
  (let [md5 (lib.shell/md5 (str file))
        [_f ext] (fs/split-ext (fs/file-name file))]
    (str md5 "." ext)))

(defn take-screenshot! [[_ url] & {:keys [width height full-page? dst]
                                   :or {width 1280
                                        height 720
                                        full-page? true
                                        dst (fs/create-temp-file {:prefix "playwright-screenshot"
                                                                  :suffix ".png"})}}]
  (try
    (let [args ["playwright" "screenshot"
                (when (and width height) (format "--viewport-size=%s,%s" width height))
                (when full-page? "--full-page")
                url
                (str dst)]]
      (apply (partial bp/shell {:out :string :err :string}) args)
      [:file (str dst)])
    (catch Exception e
      [:error (str "Exception taking screenshot: " (.getMessage e))])))

(comment
  (take-screenshot! [:url "https://naeemnur.com/side-projects/"])
  nil)

(defn attach-file [file {:keys [attach-dir]
                         :or {attach-dir clojure-attach-dir}}]
  (match file
         [:file f] (let [dest (fs/path attach-dir (md5-filename f))]
                     (fs/create-dirs attach-dir)
                     (try (fs/copy f dest) (catch java.nio.file.FileAlreadyExistsException _))
                     dest)
         :else {:error (str "Could not attach " file)}))

(defn org-link [file {:keys [attach-dir]
                      :or {attach-dir clojure-attach-dir}}]
  (let [path (fs/relativize attach-dir file)]
    (format "[[my-attach:%s]]" path)))

(defn attach-typed [typed opts]
  (-> (match typed
             [:url _] (-> (download-url typed)
                          (attach-file opts))
             [:file _] (attach-file typed opts)
             [:error e] (throw (Exception. (str "Error: " \newline e))))
      (org-link opts)))

;; Commands --------------------------------------------------------------------

(defn main [{:keys [opts]}]
  (let [{:keys [url yank screenshot _attach-dir]} opts
        [kind :as typed] (if url
                           (string->typed url)
                           (clipboard-content->typed))
        url? (= kind :url)]
    (cond-> typed
        (and url? screenshot) take-screenshot!
        :always (attach-typed opts)
        yank (doto lib.clipboard/set-clip)
        :else (doto println))))

(defn help [_]
  (let [help-str (->> ["org_attach <url>"
                       "  --yank          Copy result to clipboard"
                       "  --attach-dir    Change the attachment dir"
                       "  --screenshot    Create a screenshot using playwright to attach"]
                      (str/join "\n"))]
    (println help-str)))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["help"] :fn help}
   {:cmds [] :fn main :args->opts [:url]}])

(defn -main [& args]
  (cli/dispatch table args))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

(comment
  (attach-typed [:url "https://twitter.com/ninja_padrino/status/1711566146733052156"] {})
  (-main "https://twitter.com/ninja_padrino/status/1711566146733052156")
  (-main)
  nil)
