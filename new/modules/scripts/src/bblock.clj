(ns bblock
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]))

;; Helpers ---------------------------------------------------------------------

(defn remove-section [v start-marker end-marker]
  (let [[before section-and-after] (split-with #(not= % start-marker) v)
        after (drop-while #(not= % end-marker) (rest section-and-after))]
    (concat before (rest after))))

(remove-section [1 2 3 4 5] :foo :bar)

;; Hosts -----------------------------------------------------------------------

(def blockable-hosts
  (->> ["reddit.com"
        "www.reddit.com"
        "np.reddit.com"
        "old.reddit.com"
        "www.old.reddit.com"

        "news.ycombinator.com"
        "hckrnews.com"

        "twitter.com"
        "api.twitter.com"
        "www.twitter.com"
        "mobile.twitter.com"]
       (map #(str "127.0.0.1 " %))))

(def start-block-str "# START BBLOCK")
(def end-block-str "# END BBLOCK")

;; Commands --------------------------------------------------------------------

(defn- enable-hosts-blocking-cmd [_opts]
  (fs/write-lines "/etc/hosts"
                  (concat (-> (fs/read-all-lines "/etc/hosts")
                              (remove-section start-block-str end-block-str))
                          [start-block-str]
                          blockable-hosts
                          [end-block-str])))

(defn- disable-hosts-blocking-cmd [_opts]
  (let [lines (-> (fs/read-all-lines "/etc/hosts")
                  (remove-section start-block-str end-block-str))]
    (fs/write-lines "/etc/hosts" lines)))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["enable"] :fn enable-hosts-blocking-cmd}
   {:cmds ["disable"] :fn disable-hosts-blocking-cmd}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (-main)
  nil)
