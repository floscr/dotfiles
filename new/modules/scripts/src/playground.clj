(ns playground
  (:require
   [babashka.pods :as pods]
   [babashka.http-client :as http]
   [cheshire.core :as json]
   [hiccup-find.core :as hf]
   [clojure.string :as str]))

(require '[clojure.tools.reader.edn :as edn])

(pods/load-pod "bootleg-wrapped")
(require '[pod.retrogradeorbit.bootleg.utils :refer [convert-to]])

(defn tree-find-ns-definitions [tree]
  (->> tree
       (filter #(and (seq? %)
                     (= (first %) :require)))
       (first)
       (rest)))

(defn find-ns-require [ns' tree]
  (filter #(= ns' (first %)) tree))

(some #{} [1 2 3])

(defn find-require-refer-or-alias [tree refer-symbol]
  (when-let [ns' (->>
                  (tree-find-ns-definitions)
                  (filter #(= 'cljs.test (first %)))
                  (first))
             refer-symbol (some->> (filter #(= :refer %) ns')
                                   (first)
                                   (some #{refer-symbol}))
             alias (some->> (filter #(= :as %) ns')
                            (first)
                            (some #{refer-symbol}))]))

(defn str->tree [str]
  (edn/read-string str))

(->> (slurp "/home/floscr/Code/Work/Hyma/tokens-studio-for-penpot/frontend/test/token_tests/logic/token_actions_test.cljs")
     (str->tree)
     (tree-find-ns-definitions)
     (filter #(= 'cljs.test (first %))))
