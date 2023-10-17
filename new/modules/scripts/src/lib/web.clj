(ns lib.web
  (:require [clojure.string :as str]))

(def ^:private uri-escape-table
  {\space "%20"
   \! "%21"
   \# "%23"
   \$ "%24"
   \% "%25"
   \& "%26"
   \' "%27"
   \( "%28"
   \) "%29"
   \* "%2A"
   \+ "%2B"
   \, "%2C"
   \/ "%2F"
   \: "%3A"
   \; "%3B"
   \= "%3D"
   \? "%3F"
   \@ "%40"
   \[ "%5B"
   \] "%5D"})

(defn uri-escape [uri]
  (str/escape uri uri-escape-table))

(defn twitter-url->nitter [url]
  (str/replace url "twitter.com" "nitter.net"))
