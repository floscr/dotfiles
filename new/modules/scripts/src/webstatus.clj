(ns webstatus
  (:require
   [cheshire.core :as json]
   [babashka.http-client :as http]
   [clojure.string :as str]))

(defn query-feature [feature]
  (->> ["https://api.webstatus.dev/v1/features" feature]
       (filter some?)
       (str/join "/")
       (http/get)
       :body
       (json/parse-string)))

(defn query-features []
  (query-feature nil))

(defn features-list []
  (->> (get (query-features) "data")
       (map #(get % "feature_id"))))

;; https://github.com/GoogleChrome/webstatus.dev/blob/main/openapi/backend/openapi.yaml

(comment
  (query-feature "backdrop-filter")
  (query-features)
  (features-list)
  (query-feature "transition-behavior")
  nil)
