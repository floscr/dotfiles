(ns playground
  (:require
   [babashka.pods :as pods]
   [babashka.http-client :as http]
   [cheshire.core :as json]
   [hiccup-find.core :as hf]
   [clojure.string :as str]))

(pods/load-pod "bootleg-wrapped")
(require '[pod.retrogradeorbit.bootleg.utils :refer [convert-to]])

(defonce result (atom (http/get "https://www.willhaben.at/iad/immobilien/mietwohnungen/mietwohnung-angebote?sfId=9a0875fb-f09f-4f77-a81e-2b718079a371&isNavigation=true&areaId=117237&FREE_AREA/FREE_AREA_TYPE=20&FREE_AREA/FREE_AREA_TYPE=30&page=2&rows=30&ESTATE_SIZE/LIVING_AREA_FROM=70")))

(defonce hiccup-result (atom nil))

(defn slurp-data []
  (let [res @result
        hiccup (convert-to (:body res) :hiccup)
        [_ _ data] (-> (hf/hiccup-find-1 [:#__NEXT_DATA__] hiccup)
                       (first))
        json-edn (json/parse-string data)]
    (reset! hiccup-result json-edn)
    (spit "data.json" data)
    (spit "data.edn" json-edn)))

(defn json-path->clojure-path [path]
  (-> (str/split path #"\.")
      (rest)
      (vec)))

(comment
  (json-path->clojure-path ".props.pageProps.searchResult.advertSummaryList.advertSummary")
  nil)

(defn collect-items [data json-path]
  (let [data-path (json-path->clojure-path json-path)
        items (get-in data data-path)]
    items))


(->> (collect-items
      @hiccup-result
      ".props.pageProps.searchResult.advertSummaryList.advertSummary")
     (map (fn [x]
            (-> (select-keys x #{"id" "attributes"})
                (update "attributes"
                        (fn [attrs]
                          (some->> (get attrs "attribute")
                                   (map (fn [x] [(get x "name") (get x "values")]))
                                   (into {}))))))))

(-> (slurp "/home/floscr/Code/Projects/org-web/projects/frontend/src/app/plt.json")
    (json/parse-string keyword))
