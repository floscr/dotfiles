(ns lib.math)

(defn clamp [value min-value max-value]
  (cond
    (< value min-value) min-value
    (> value max-value) max-value
    :else value))
