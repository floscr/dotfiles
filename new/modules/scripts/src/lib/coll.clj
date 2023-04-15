(ns lib.coll)

(defn get-first
  "Returns the first available key from `keys` in `coll`."
  [keys coll]
  (->> (filter #(get coll %) keys)
       (first)
       (get coll)))

(comment
  (get-first [:first :second :third] {:third 3})
  ;; => 3
  (get-first [:first] {:third 3})
  ;; => nil
  nil)
