(ns lib.notifications
  (:require
   [babashka.process :as bp]))

(def opts->args-map {:clear-after-s #(format "--timeout=%d" (* % 1000))
                     :error? (constantly "--urgency=critical")})

(defn- opts->args [opts]
  (mapv
   (fn [[k v]] (-> (get opts->args-map k)
                   (apply [v])))
   opts))

(defn show
  ([msg]
   (show msg {}))
  ([msg opts]
   (bp/sh (concat ["dunstify" msg] (opts->args opts)))))

(defn error
  ([msg]
   (error msg {}))
  ([msg opts]
   (show msg (assoc opts :error? true))))

(comment
  (show "Hello world")
  (show "Hello world" {:clear-after-s 1})
  (error "Hello world")
  (bp/sh "dunstify" "--timeout=3000" "\"Hello world\"")
  nil)
