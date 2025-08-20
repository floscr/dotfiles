(ns lib.str)

(defn drop-char [s]
  (if (seq s)
    (subs s 0 (dec (count s)))
    ""))
