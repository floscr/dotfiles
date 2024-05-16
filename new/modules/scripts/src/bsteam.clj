(ns bsteam
  (:require
   [babashka.fs :as fs]
   [clojure.pprint :refer [pprint]]
   clojure.walk
   [lib.acf-parser :as acf-parser]
   [lib.num :as num]
   [lib.xdg :as xdg]))

;; Helpers ---------------------------------------------------------------------

(defn steam-apps-dir []
  (xdg/data-path "Steam/steamapps"))

(def ignore-steam-apps-with-name
  "Ignore these steam applications"
  #{"Steamworks Common Redistributables"
    "Steam Linux Runtime 2.0 (soldier)"
    "Steam Linux Runtime 3.0 (sniper)"
    "Proton Experimental"
    "Proton 7.0"})

(defn acf->game-map [acf]
  {:name (get-in acf ["AppState" "name"])
   :id (get-in acf ["AppState" "appid"])
   :last-played (some-> (get-in acf ["AppState" "LastPlayed"])
                        (num/safe-parse-int))})

(defn list-steam-games []
  (->> (fs/list-dir (steam-apps-dir) "*.acf")
       (eduction
        (map (comp acf->game-map acf-parser/parse slurp str))
        (remove #(ignore-steam-apps-with-name (:name %))))
       (sort-by :last-played)
       (reverse)))

;; Main ------------------------------------------------------------------------

(defn -main [& args]
  (-> (list-steam-games)
      (doto pprint)))

(apply -main *command-line-args*)

(comment
  (-main)
  nil)
