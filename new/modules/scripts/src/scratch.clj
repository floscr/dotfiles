(ns scratch
  (:require
   [babashka.process :as bp]
   [cheshire.core :as json]))

(-> (bp/shell "echo foo")
    (doto println))

;; Binding to
(defn list-open-issues [repo]
  (-> (bp/sh (format "gh issue list --json url -R %s" repo))
      :out
      (json/parse-string true)))

(defn transfer-issue [issue-url repo]
  (bp/sh (format "gh issue transfer %s %s" issue-url repo)))

;; List all issues and take 1 out to verify everything works
(let [from-repo "tokens-studio/tokens-studio-for-penpot"
      to-repo "tokens-studio/penpot-temp-issues"]
  (->> (list-open-issues from-repo)
       (map :url)
       (take 1)
       (mapv #(:out (transfer-issue % to-repo)))))

;; List all issues and take 1 out to verify everything works
(let [from-repo "tokens-studio/penpot-temp-issues"
      to-repo "tokens-studio/penpot"]
  (->> (list-open-issues from-repo)
       (map :url)
       (mapv (fn [x]
               (prn "Transfering issue: " x)
               (-> (:out (transfer-issue x to-repo))
                   (doto println))))))
