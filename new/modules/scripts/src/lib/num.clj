(ns lib.num)

(defn parse-int [str]
  (Integer/parseInt str))

(defn safe-parse-int [val]
  (try (Integer/parseInt val)
     (catch Exception _ nil)))
