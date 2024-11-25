(ns bifi
  (:require
   [babashka.process :as bp]))

;; Main ------------------------------------------------------------------------

(defn -main [& args]
  (bp/sh "nmcli connection up uuid 30d8f8e5-bde8-49d3-90d4-c986fa02659d"))

(apply -main *command-line-args*)

(comment
  (-main)
  nil)
