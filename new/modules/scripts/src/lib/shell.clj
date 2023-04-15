(ns lib.shell
  (:require
   [babashka.process :as bp]
   [clojure.string :as str]))

(defmacro bold [str]
  `(str "\033[1m" ~str "\033[0m"))

(defn sh
  "Wrapper around `babashka.process/sh` that returns `:out` or `nil` when `cmd` failed."
  ([cmd] (sh cmd nil))
  ([cmd opts]
   (let [{:keys [exit out _err]} (if opts
                                   (bp/sh cmd opts)
                                   (bp/sh cmd))]
     (when (zero? exit) out))))

(defn lines
  ([cmd] (lines cmd nil))
  ([cmd opts]
   (some-> (sh cmd opts)
           (str/trim)
           (str/split-lines))))

(defn md5 [path]
  (when-let [[result] (lines ["md5sum" path])]
    (-> (str/split result #" ")
        first)))
