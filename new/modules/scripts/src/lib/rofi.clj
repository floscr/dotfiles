(ns lib.rofi
  (:require
   [babashka.process :as bp]
   [clojure.string :as str]
   [lib.num :as num]))

(def default-args ["-i" ;; Case Insensitive filter
                   "-levenshtein-sort"])

(defn select [items & {:keys [to-title prompt args]
                       :or {to-title str
                            args default-args}}]
  (let [in (->> items
                (map to-title)
                (str/join "\n"))]
    (some-> (bp/sh (concat ["rofi" "-dmenu" "-format" "i"] args (when prompt ["-p" prompt]))
                   {:in in :continue true})
            :out
            (str/trim)
            (num/safe-parse-int)
            (#(nth items %)))))

(comment
  (select [{:title "foo"}] {:to-title :title})
  nil)
