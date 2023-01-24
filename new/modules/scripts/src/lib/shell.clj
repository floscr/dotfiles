(ns lib.shell
  (:require
   [babashka.process :as bp]
   [clojure.string :as str]))

(defmacro bold [str]
  `(str "\033[1m" ~str "\033[0m"))

(defn sh
  "Wrapper around `babashka.process/sh` that returns `:out` or `nil` when `cmd` failed."
  ([cmd] (sh cmd nil nil))
  ([cmd opts] (sh cmd opts nil))
  ([cmd opts prev]
   (let [{:keys [exit out err]} (bp/sh cmd opts prev)]
     (when (= exit 0)
       out))))

(defn sh-lines
  ([cmd] (sh-lines cmd nil nil))
  ([cmd opts] (sh-lines cmd opts nil))
  ([cmd opts lines]
   (some-> (sh cmd opts lines)
           (str/trim)
           (str/split-lines))))
