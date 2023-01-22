(ns lib.shell
  (:require
   [clojure.string :as str]))

(defmacro bold [str]
  `(str "\033[1m" ~str "\033[0m"))
