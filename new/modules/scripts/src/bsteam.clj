(ns bsteam
  (:require
   [babashka.fs :as fs]
   clojure.walk
   [lib.acf-parser :as acf-parser]))

;; Helpers ---------------------------------------------------------------------

(defn steam-apps-dir []
  (fs/path (System/getenv "XDG_DATA_HOME") "Steam/steamapps"))

(defn all-steam-games []
  (let [configs (-> (steam-apps-dir)
                    (fs/list-dir "*.acf"))]
    configs))

(comment
  (-> (all-steam-games)
      (first))
  ;; => #object[sun.nio.fs.UnixPath 0x372084dd "/home/floscr/.local/share/Steam/steamapps/appmanifest_228980.acf"]

  (->> (all-steam-games)
       (first)
       (str)
       (slurp)
       (acf-parser/parse))

  nil)

(defn run []
  ())

;; Main ------------------------------------------------------------------------

(defn -main [& args])

(apply -main *command-line-args*)

(comment
  (-main)
  nil)
