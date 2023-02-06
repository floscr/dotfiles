(ns lib.fs
  (:require [babashka.fs :as fs]))

(defn expand
  "Wrapper around `babashka.fs/expand-home`.
  Directly returns the string."
  [path]
  (-> path fs/expand-home str))
