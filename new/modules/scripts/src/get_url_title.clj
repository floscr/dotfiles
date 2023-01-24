(ns get_url_title
  (:require
   [babashka.pods :as pods]
   [clojure.core.match :refer [match]]
   [babashka.curl :as curl]
   [babashka.fs :as fs]
   [babashka.cli :as cli]))

;; HTML Parsing with jsoup pod binary
(pods/load-pod (-> (fs/expand-home "~/.config/dotfiles/new/modules/scripts/deps/pod-jaydeesimon-jsoup.bin") (str)))
(require '[pod.jaydeesimon.jsoup :as jsoup])

(defn exit! []
  (System/exit 1))

(defn get-title [url]
  (match
   (curl/get url {:throw false})
   {:status 200 :body body} (some-> body (jsoup/select "title") first :text)
   :else nil))

(comment
  (get-title "https://matthiasott.com/notes/the-thing-with-leading-in-css")
  ;; Github
  (get-title "https://github.com/rafaelsgirao/dotfiles/blob/0932e73b6b180b6f12c0cf69c5ee3898fa6bf8b0/nixos/modules/graphical.nix#L158")
  ;; Youtube
  (get-title "https://www.youtube.com/watch?v=bXRDfxp_4H0")
  nil)

;; Commands --------------------------------------------------------------------

(defn help! [_]
  (println "Help"))

(defn main! [{:keys [opts]}]
  (let [{:keys [url]} opts]
    (if (empty? url)
      (help! {})
      (if-let [title (get-title url)]
        (do (println title)
            title)
        (do
          (println "Could not get title for url: " url)
          (exit!))))))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds [] :args->opts [:url] :fn main!}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (-main "https://matthiasott.com/notes/the-thing-with-leading-in-css")

  (with-redefs [exit! (fn [] "exit")]
    (-main "https://httpstat.us/404"))

  nil)
