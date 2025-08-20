(ns tty
  (:require [babashka.process :as process]
            [clojure.java.io :as io]))

(defn p [& ss] (apply print ss) (flush))
(defn pl [& ss] (apply println ss) (flush))

(defn read-key []
  (.read (io/reader (.reader (System/console)))))

(defn read-char-by-char! []
  (process/shell "stty" "-icanon" "-echo"))

(defn read-line-by-line! []
  (process/shell "stty" "icanon" "echo"))

;; ANSI versions (no subprocess, safe in shutdown)
(defn hide-cursor! [] (p "\033[?25l"))
(defn show-cursor! [] (p "\033[?25h"))
(defn enter-alternate-screen-mode [] (p "\033[?1049h"))
(defn exit-alternate-screen-mode [] (p "\033[?1049l"))
(defn clear-screen [] (p "\033[H\033[2J"))

(def default-ctrls
  {"discard" "^O"
   "dsusp"   "^Y"
   "eof"     "^D"
   "intr"    "^C"
   "kill"    "^U"
   "lnext"   "^V"
   "reprint" "^R"
   "start"   "^Q"
   "status"  "^T"
   "stop"    "^S"
   "susp"    "^Z"
   "werase"  "^?"})

(defn restore-ctrl-bindings []
  (apply process/sh "stty"
         (mapcat (fn [[ctrl binding]] [ctrl binding]) default-ctrls)))

(defn disable-ctrl-bindings []
  (apply process/sh "stty"
         (mapcat (fn [[ctrl _]] [ctrl "''"]) default-ctrls)))

(def key-aliases
  {127 :backspace
   8   :backspace
   10  :enter
   3   :ctrl-c
   4   :ctrl-d})

(def quit-alias? #{:ctrl-c :ctrl-d})

(defn interpret-key [k]
  (get key-aliases k (char k)))

;; only ANSI here (safe in shutdown hook)
(defn soft-exit! []
  (clear-screen)
  (show-cursor!)
  (exit-alternate-screen-mode))

;; full reset (used when you quit normally)
(defn exit! []
  (soft-exit!)
  (read-line-by-line!)
  (restore-ctrl-bindings))

(defn add-shutdown-hook! []
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. ^Runnable soft-exit!)))

(defn start! []
  (read-char-by-char!)
  (enter-alternate-screen-mode)
  (clear-screen)
  (disable-ctrl-bindings)
  (add-shutdown-hook!))
