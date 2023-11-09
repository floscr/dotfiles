(ns lib.fp)

(defn apply-fns
  "Apply list of `fns` to a value `x`."
  [x fns]
  (reduce (fn [v f] (f v)) x (vec fns)))

(comment
  (apply-fns {:foo 1} [:foo str])
  nil)

(defn keep-> [x pred]
  (when (pred x) x))

(defn keep->> [pred x]
  (when (pred x) x))

(defn discard-> [x pred]
  (when-not (pred x) x))

(defn discard->> [pred x]
  (when-not (pred x) x))
