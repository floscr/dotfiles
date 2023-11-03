(ns convert-html
  (:require
   [babashka.pods :as pods]
   [clojure.string :as str]))

(pods/load-pod "bootleg-wrapped")
(require '[pod.retrogradeorbit.bootleg.utils :refer [convert-to]])

(defn ->hiccup [svg]
  (->
   ;; Wrap in html otherwise hickory won't parse
   (str "<html>" svg "</html>")
   ;; Remove whitespace
   (str/replace #"\n\s*" "")
   (convert-to :hiccup)
   ;; Discard html hiccup wrapper
   (second)))

(defn hiccup->str [tree]
  (letfn [(convert-node [node]
            (if (vector? node)
              (str "($ " (first node)
                   (if (map? (second node))
                     (str " " (pr-str (second node)))
                     "")
                   (if (next node)
                     " "
                     "")
                   (apply str (map convert-node (rest (next node))))
                   ")")
              (pr-str node)))]
    (convert-node tree)))

(defn ->uix [svg]
  (->> (->hiccup svg)
       (hiccup->str)))

(-> (slurp *in*) (->uix) prn)
