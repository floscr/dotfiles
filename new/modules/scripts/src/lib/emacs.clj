(ns lib.emacs
  (:require
   [babashka.process :as bp]))

(defn open-file! [file-path & {:keys [window? opts]}]
  (bp/shell opts ["emacsclient" (if window? "-nw" "-n") file-path]))
