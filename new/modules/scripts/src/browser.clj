(ns browser
  (:require
   [babashka.cli :as cli]
   [babashka.process :as p]
   [clojure.string :as str]))

;; Config ----------------------------------------------------------------------

(def browsers
  {:firefox {:cmd "firefox" :args []}
   :brave {:cmd "brave" :args []}
   :chromium {:cmd "chromium" :args []}})

(def config
  {:default-browser :firefox
   :browsers browsers})

;; Helpers ---------------------------------------------------------------------

(defn get-browser-cmd [browser]
  (get-in config [:browsers browser :cmd]))

(defn get-browser-args [browser]
  (get-in config [:browsers browser :args] []))

(defn launch-browser
  "Launch the specified browser with the given URL and optional args"
  [browser url & extra-args]
  (let [cmd (get-browser-cmd browser)
        args (concat (get-browser-args browser) extra-args [url])]
    (when cmd
      (apply p/shell cmd args))))

;; Browser Selection Logic ----------------------------------------------------

(defn select-browser
  "Dynamically select browser based on URL or other criteria.
   You can customize this function to implement your own logic."
  [url]
  (cond
    ;; Example: Use Chromium for specific sites
    (re-find #"meet\.google\.com|teams\.microsoft\.com" url)
    :chromium

    ;; Example: Use Brave for YouTube
    (re-find #"youtube\.com|youtu\.be" url)
    :brave

    ;; Default to Firefox
    :else
    (:default-browser config)))

;; Commands --------------------------------------------------------------------

(defn open-cmd [{:keys [opts args]}]
  (let [url (first args)
        browser-key (or (when-let [b (:browser opts)]
                         (keyword b))
                       (select-browser url))
        browser (get browsers browser-key (:default-browser config))]
    (if url
      (do
        (println (str "Opening " url " with " (name browser-key)))
        (launch-browser browser-key url))
      (println "Error: No URL provided"))))

(defn list-cmd [_]
  (println "Available browsers:")
  (doseq [[k v] (:browsers config)]
    (println (str "  " (name k) ": " (:cmd v)))))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds [] :fn open-cmd}
   {:cmds ["open"] :fn open-cmd}
   {:cmds ["list"] :fn list-cmd}])

(defn -main [& args]
  (cli/dispatch table args {:coerce {:browser :keyword}}))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

(comment
  (-main "list")
  (-main "https://github.com")
  (-main "open" "--browser" "brave" "https://youtube.com")
  nil)
