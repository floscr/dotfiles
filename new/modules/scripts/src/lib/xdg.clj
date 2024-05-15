(ns lib.xdg
  (:require
   [babashka.fs :as fs]))

(defn path [xdg-var args]
  (when-let [xdg-path (System/getenv xdg-var)]
    (apply fs/path xdg-path args)))

(defn cache-path [& args]
  (path "XDG_CACHE_HOME" args))

(defn config-path [& args]
  (path "XDG_CONFIG_HOME" args))

(defn data-path [& args]
  (path "XDG_DATA_HOME" args))

(comment
  (cache-path)
  (config-path)
  (data-path)
  nil)
