(ns lib.emacs
  (:require
   [babashka.process :as bp]))

(defn open-file! [file-path & {:keys [window? opts]}]
  (bp/shell opts "emacsclient" (if window? "-nw" "-n") file-path))

(defn run-cmd! [cmd & {:keys [opts]}]
  (bp/shell opts "emacsclient" "-e" cmd))

(comment
  (require '[clojure.string :as str])

  (open-file! ".")
  (open-file! "." {:opts {:dir "/tmp"}})
  (open-file! "." {:window? true})

  (run-cmd! "(message \"foo\")")
  nil)
