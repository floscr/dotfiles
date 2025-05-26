(ns bplaywright
  "Wrapped playwright"
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [babashka.process :as bp]
   [cheshire.core :as json]
   [clojure.string :as str]))

;; Helpers ---------------------------------------------------------------------

(defn find-linux-chrome-executable [path]
  (let [executable (volatile! nil)]
    (fs/walk-file-tree path
                       {:follow-links true
                        :max-depth 5
                        :visit-file
                        (fn [path _]
                          (if (and (fs/regular-file? path)
                                   (fs/executable? path)
                                   (str/starts-with? (fs/file-name path) "chrom"))
                            (do
                              (vreset! executable path)
                              :terminate)
                            :continue))})
    @executable))

(comment
  (find-linux-chrome-executable "/nix/store/n9mw415x39km3z6xl92bqsrgmzxaiagv-playwright-browsers")
  nil)

(defn find-playwright-chromium-executable
  "Find chromium executable installed through nix flake.
  E.g.: /nix/store/n9mw415x39km3z6xl92bqsrgmzxaiagv-playwright-browsers/chromium-1134/chrome-linux/chrome"
  [browsers-path]
  (when-let [executable (find-linux-chrome-executable browsers-path)]
    (let [revision (->> (fs/parent executable)
                        (fs/parent)
                        (#(str/split (str %) #"-"))
                        (last))
          version (->> (bp/shell {:out :string} (str executable) "--version")
                       :out
                       (#(str/split % #" "))
                       (filter #(re-matches #"[0-9\.]+" %))
                       (first))]
      {:executable executable
       :revision revision
       :version version})))

(defn nixos-find-playwright-chromium-executable []
  (-> (System/getenv "PLAYWRIGHT_BROWSERS_PATH")
      (find-playwright-chromium-executable)))

(defn find-browsers-json []
  (->> ["./node_modules/playwright/node_modules/playwright-core/browsers.json"
        "./node_modules/playwright-core/browsers.json"]
       (filter fs/exists?)
       (first)))

(defn fix-playwright-chromium-version!
  "Playwright doesn't allow setting a chrome executable and also not passing a custom config...
  So we override the playwright browsers config to fit the Nixos browser version."
  [{:keys [revision version] :as _browser-config}]
  (let [browsers-json-path (or (find-browsers-json)
                               (throw (Exception. "Could not find browsers-json")))
        fixed-json (-> (slurp browsers-json-path)
                       (json/parse-string)
                       (update "browsers" (fn [browsers]
                                            (->> browsers
                                                 (map (fn [browser]
                                                        (if (or
                                                             (= (get browser "name") "chromium")
                                                             (= (get browser "name") "chromium-headless-shell")
                                                             (= (get browser "name") "chromium-tip-of-tree"))
                                                          {"name" (get browser "name")
                                                           "revision" revision
                                                           "installByDefault" false
                                                           "browserVersion" version}
                                                          browser)))))))]
    (spit browsers-json-path (json/generate-string fixed-json {:pretty true}))))

(comment
  (find-linux-chrome-executable "/nix/store/i0cxsz5wc9y7jagjb2yrj2w90kz857p7-playwright-browsers")
  (find-playwright-chromium-executable "/nix/store/i0cxsz5wc9y7jagjb2yrj2w90kz857p7-playwright-browsers")
  ,)

(defn nixos-fix-playwright! []
  (-> (nixos-find-playwright-chromium-executable)
      (fix-playwright-chromium-version!)))

;; Commands --------------------------------------------------------------------

(defn run-wrapped-cmd
  "Fix playwright and then run the provided command with the necessary environment variables"
  [{:keys [opts args]}]
  (println "Fixing playwright...")
  (nixos-fix-playwright!)

  (println "Running command with playwright environment variables...")
  (let [browser-config (nixos-find-playwright-chromium-executable)
        executable (:executable browser-config)
        env-vars {"PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS" "1"
                  "PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD" "1"}
        cmd (str/join " " args)]

    (println "Using browser executable:" (str executable))
    (println "Environment variables:" env-vars)
    (println "Running command:" cmd)

    (bp/shell {:extra-env env-vars} cmd)))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["fix"] :fn (fn [_] (nixos-fix-playwright!))}
   {:cmds ["run-wrapped"] :fn run-wrapped-cmd}])

(defn dispatch [& args]
  (try
    (cli/dispatch table args)
    (catch Exception e
      (let [err-type (:type (ex-data e))]
        (cond
          (= :babashka.process/error err-type)
          (let [{:keys [exit]} (ex-data e)]
            ;; Assume that the subprocess has already printed an error message
            (System/exit exit))
          :else
          (throw e))))))

(defn -main [& args]
  (apply dispatch args))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

(comment
  (-main)
  nil)
