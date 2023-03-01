(ns lib.fp)

(defn keep-> [x pred]
  (when (pred x) x))

(defn keep->> [pred x]
  (when (pred x) x))

(defn discard-> [x pred]
  (when-not (pred x) x))

(defn discard->> [pred x]
  (when-not (pred x) x))
