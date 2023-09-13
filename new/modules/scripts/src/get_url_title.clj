(ns get_url_title
  (:require
   [babashka.cli :as cli]
   [babashka.curl :as curl]
   [babashka.fs :as fs]
   [babashka.pods :as pods]
   [cheshire.core :as json]
   [clojure.core.match :refer [match]]
   [clojure.string :as str]
   [lambdaisland.uri :as uri]
   [lib.shell :refer [sh]]
   [lib.web :as lw]))

;; HTML Parsing with jsoup pod binary
(pods/load-pod (-> (fs/expand-home "~/.config/dotfiles/new/modules/scripts/deps/pod-jaydeesimon-jsoup.bin") (str)))
(require '[pod.jaydeesimon.jsoup :as jsoup])

(defn exit! []
  (System/exit 1))

(defn gh-get-pr-title [url path]
 (when-let [{:keys [title author number]} (some-> (sh (format "gh pr view %s --json title,author,number" url))
                                                  (json/parse-string keyword))]
   (let [[_ user repository] (str/split path #"/")]
     (str "PR from " (:name author) ": " title
          " (" user "/" repository "/" "#" number ")"))))

(defn curl-get-title [url]
  (match (curl/get url {:throw false})
         {:status 200 :body body} (some-> body (jsoup/select "title") first :text)
         :else nil))

(defn twitter->thread-reader [url]
  (let [threadreader-url (str "https://threadreaderapp.com/search?q=" (lw/uri-escape url))]
    (match (curl/get threadreader-url)
           {:status 200 :body body} (let [[{:keys [attrs text]}] (some-> body (jsoup/select "#tweet_1"))]
                                      [(format "Tweet by %s: %s" (get attrs "data-screenname") text) threadreader-url])
           :else [url threadreader-url])))

(defn get-title [url]
  (match [(uri/uri url)]
         [{:host "github.com" :path path} :guard [#(some-> (:path %) (str/includes? "/pull"))]] [(gh-get-pr-title url path)]
         [{:host "twitter.com"}] (twitter->thread-reader url)
         :else [(curl-get-title url)]))

(comment
  (get-title "https://httpstat.us")
  ;; => ["httpstat.us"]
  (get-title "https://twitter.com/mattpocockuk/status/1625838626742435842")
  ;; => ["Tweet by mattpocockuk: If you don't know generics, I promise you'll understand them by the end of this thread. I like a challenge." "https://threadreaderapp.com/search?q=https%3A%2F%2Ftwitter.com%2Fmattpocockuk%2Fstatus%2F1625838626742435842"]
  (get-title "https://github.com/NixOS/nixpkgs/pull/214898")
  ;; => ["PR from R. RyanTM: babashka: 1.1.172 -> 1.1.173 (NixOS/nixpkgs/#214898)"]
  (get-title "https://matthiasott.com/notes/the-thing-with-leading-in-css")
  ;; => ["The Thing With Leading in CSS · Matthias Ott – User Experience Designer"]
  (curl-get-title "https://github.com/rafaelsgirao/dotfiles/blob/0932e73b6b180b6f12c0cf69c5ee3898fa6bf8b0/nixos/modules/graphical.nix#L158")
  ;; => "dotfiles/graphical.nix at 0932e73b6b180b6f12c0cf69c5ee3898fa6bf8b0 · rafaelsgirao/dotfiles · GitHub"
  (curl-get-title "https://www.youtube.com/watch?v=bXRDfxp_4H0")
  ;; => "Through the looking glass w Data Rabbit: \"A System built for Seeing\" (by Ryan Robitaille) - YouTube"
  (curl-get-title "https://github.com/floscr/dotfiles/blob/07156f2d059da2676d13d17d67eb8fed596557db/new/modules/scripts/src/get_url_title.clj")
  nil)

(defn get-rss [url]
  (match
   (curl/get url {:throw false})
   {:status 200 :body body} (when-let [link (some-> body
                                                    (#(or (some-> (jsoup/select % "[type='application/atom+xml']") (first))
                                                          (some-> (jsoup/select % "[type='application/rss+xml']") (first))
                                                          (some-> (jsoup/select % "[type='application/+xml']") (first))))
                                                    (get-in [:attrs "href"]))]
                              (cond
                                (-> link uri/uri :host) link
                                :else (let [{:keys [scheme host]} (uri/uri url)]
                                        (str scheme "://" host (when-not (str/starts-with? link "/") "/") link))))
   :else nil))

(defn org-link
  [url title]
  (if title
    (format "[[%s][%s]]" url title)
    (format "[[%s]]" url)))

(comment
  (get-rss "https://dawranliou.com/blog/")
  ;; => "https://dawranliou.com/atom.xml"
  (get-rss "http://yummymelon.com/devnull/")
  ;; => "http://yummymelon.com/devnull/feeds/all.atom.xml"
  (get-rss "https://rattlin.blog/bbgum.html")
  ;; => "https://rattlin.blog/atom.xml"
  nil)

;; Commands --------------------------------------------------------------------

(defn help! [_]
  (println "Help"))

(defn get-rss-cmd [{:keys [opts]}]
  (let [{:keys [url org]} opts]
    (if (empty? url)
      (help! {})
      (let [title (get-rss url)
            title-str (if org (org-link title url) title)]
        (if title-str
          (do (println title-str)
              title-str)
          (do
            (println "Could not get rss feed for url: " url)
            (exit!)))))))

(defn get-url-cmd [{:keys [opts] :as args}]
  (let [{:keys [url org]} opts]
    (if (empty? url)
      (help! {})
      (let [[title adapted-url] (get-title url)
            url* (or adapted-url url)
            title-str (if org (org-link url* title) title)]
        (if title-str
          (do (println title-str)
              title-str)
          (do
            (println "Could not get title for url: " url*)
            (exit!)))))))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["url"] :args->opts [:url] :fn get-url-cmd}
   {:cmds ["rss"] :args->opts [:url] :fn get-rss-cmd}
   {:cmds [] :args->opts [:url] :fn get-url-cmd}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (-main "https://matthiasott.com/notes/the-thing-with-leading-in-css")

  (with-redefs [exit! (fn [] "exit")]
    (-main "https://httpstat.us/404"))

  nil)
