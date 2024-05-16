(ns bsteam
  (:require
   [babashka.fs :as fs]
   clojure.walk
   [lib.acf-parser :as acf-parser]
   [lib.num :as num]
   [lib.xdg :as xdg]))

;; Helpers ---------------------------------------------------------------------

(defn steam-apps-dir []
  (xdg/data-path "Steam/steamapps"))

(defn all-steam-games []
  (let [configs (->> (fs/list-dir (steam-apps-dir) "*.acf"))]
    configs))

(defn acf->game-map [acf]
  {:name (get-in acf ["AppState" "name"])
   :id (get-in acf ["AppState" "appid"])
   :last-played (some-> (get-in acf ["AppState" "LastPlayed"])
                        (num/safe-parse-int))})

(def ignore-steam-apps-with-name #{"Steamworks Common Redistributables"
                                   "Steam Linux Runtime 2.0 (soldier)"
                                   "Steam Linux Runtime 3.0 (sniper)"
                                   "Proton Experimental"
                                   "Proton 7.0"})

(comment
  (->> (all-steam-games)
       (eduction
        (map (comp acf->game-map acf-parser/parse slurp str))
        (remove #(ignore-steam-apps-with-name (:name %))))
       (sort-by :last-played)
       (reverse))
  nil)

(defn run []
  ())

;; Main ------------------------------------------------------------------------

(defn -main [& args])

(apply -main *command-line-args*)

(comment
  (-main)
  nil)
