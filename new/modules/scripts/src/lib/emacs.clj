(ns lib.emacs
  (:require
   [babashka.process :as bp]))

(defn open-file! [file-path & {:keys [window? opts]}]
  (bp/sh opts "emacsclient" (if window? "-nw" "-n") file-path))

(defn run-cmd! [cmd & {:keys [opts]}]
  (bp/sh opts "emacsclient" "-e" cmd))

(comment
  (open-file! ".")
  (open-file! "." {:opts {:dir "/tmp"}})
  (open-file! "." {:window? true})

  (run-cmd! "(message \"foo\")")
  (run-cmd! "aaa")
  nil)
