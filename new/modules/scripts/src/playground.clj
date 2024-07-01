(ns playground
  (:require
   [babashka.pods :as pods]
   [babashka.http-client :as http]
   [cheshire.core :as json]
   [hiccup-find.core :as hf]
   [clojure.string :as str]))

(pods/load-pod "bootleg-wrapped")
(require '[pod.retrogradeorbit.bootleg.utils :refer [convert-to]])
