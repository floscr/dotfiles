(ns lib.notifications
  (:require
   [babashka.process :as bp]))

(def opts->args-map {:clear-after-s #(format "--timeout=%d" (* % 1000))})

(defn- opts->args [opts]
  (mapv
   (fn [[k v]] (-> (get opts->args-map k)
                   (apply [v])))
   opts))

(defn show [msg & {:as opts}]
  (bp/sh (concat ["dunstify" msg] (opts->args opts))))

(comment
  (show "Hello world" {:clear-after-s 1})
  (bp/sh "dunstify" "--timeout=3000" "\"Hello world\"")
  nil)
