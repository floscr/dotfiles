(ns lib.hyma.penpot.core
  (:require
   [babashka.process :as bp]
   [cheshire.core :as json]))

(def wip-status-labels
  #{"🚧  In Progress" "📨 Ready for work"})

(def wip-status-label?
  (partial contains? wip-status-labels))

(defn fetch-project-data []
  (-> (bp/sh "gh project item-list 69 --owner tokens-studio --format 'json'")
      :out
      (json/parse-string true)))

(defn project-wip-items []
  (when-let [items (-> (fetch-project-data)
                       (:items))]
    (into [] (filter (comp wip-status-label? :status) items))))

(comment
  (project-wip-items)
  nil)

