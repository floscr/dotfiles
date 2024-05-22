(ns lib.num)

(defn parse-int [str]
  (Integer/parseInt str))

(defn safe-parse-int [val]
  (try (parse-int val)
       (catch Exception _ nil)))

(defn parse-float [str]
  (Float/parseFloat str))

(defn safe-parse-float [val]
  (try (parse-float val)))
