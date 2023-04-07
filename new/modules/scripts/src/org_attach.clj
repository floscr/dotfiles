(ns org-attach
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [babashka.http-client :as http]
   [clojure.core.match :refer [match]]
   [clojure.java.io :as io]
   [clojure.string :as str]
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


((comp str/upper-case str/lower-case) "Foo")
((comp fs/regular-file? fs/expand-home last)
 [:string "/home/floscr/Documents/Org/.attach/3c/0d6094-64bc-4ca9-9858-a5402ce6eeb3/_20220216_173724EaQLXxTUMAAJQel.jpeg"])


(comment

  (-> (clipboard/content-or-file)
      (apply (comp last))
      fs/expand-home
      fs/regular-file?)
  (detect-clipboard-contents)

  (-> "/home/floscr/Documents/Org/.attach/3c/0d6094-64bc-4ca9-9858-a5402ce6eeb3/_20220216_173724EaQLXxTUMAAJQel.jpeg"
      fs/expand-home
      (#(vec [:file (fs/file %)])))
  nil)


(defn- detect-clipboard-contents []
  (let [clip (clipboard/content-or-file)]
    (match clip
           ([:string x] :guard (comp scheme? last)) [:url x]
           ([:string x] :guard (comp fs/regular-file? fs/expand-home last)) (-> x
                                                                                (fs/expand-home)
                                                                                (#(vec [:file (fs/file %)])))
           [:file x] [:file x]
           :else [:error (str "Could not find matching schema for clipboard" \newline clip)])))

(defn download-url [[_ url]]
  (let [resp (http/get url {:throw false
                            :as :stream})]
    (match resp
           {:status 200 :headers headers :body body} (let [content-type (get headers "content-type")
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
                     (fs/copy f dest)
                     dest)
         :else {:error (str "Could not attach " file)}))

(defn org-link [file]
  (let [path (fs/relativize clojure-attach-dir file)]
    (format "[[my-attach:%s]]" path)))

(comment
  (defonce file (atom (download-url [nil "https://github.com/babashka/babashka/raw/master/logo/icon.png"])))
  @file

  (-> (attach-file @file) (org-link))
  (md5-filename (last @file))

  (fs/split-ext (fs/file-name (last @file)))

  (= (lib.shell/md5 (lib.fs/expand "~/Downloads/foo"))
     (lib.shell/md5 (str (last @file))))

  (fs/copy (last @file) (lib.fs/expand "~/Downloads/foo"))
  nil)

(defn attach-clipboard []
  (let [clip (detect-clipboard-contents)]
    (-> (match clip
               [:url _] (-> (download-url clip)
                            (attach-file))
               [:file _] (attach-file clip)
               [:error e] (throw (Exception. (str "Error: " \newline e))))
        (org-link))))

;; Commands --------------------------------------------------------------------

(defn main [_args]
  (-> (attach-clipboard)
      (doto lib.clipboard/set-clip)
      (doto println)))

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
