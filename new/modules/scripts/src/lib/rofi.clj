(ns lib.rofi
  (:require
   [babashka.process :as bp]
   [clojure.string :as str]
   [lib.num :as num]))

(defn select [items & {:keys [to-title]
                       :or {to-title str}}]
  (let [in (->> items
                (map to-title)
                (str/join "\n"))]
    (some-> (bp/sh ["rofi" "-i" "-levenshtein-sort" "-dmenu" "-p" "Play" "-format" "i"]
                   {:in in :continue true})
            :out
            (str/trim)
            (num/safe-parse-int)
            (#(nth items %)))))

(comment
  (select [{:title "foo"}] {:to-title :title})
  nil)
