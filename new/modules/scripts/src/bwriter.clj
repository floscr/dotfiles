(ns bwriter
  (:require
   [lib.str]
   [lib.tty]))

(defn print-screen-loop []
  (loop [s ""]
    (let [k (lib.tty/read-key)
          k' (lib.tty/interpret-key k)
          s' (case k'
               :backspace (lib.str/drop-char s)
               (str s (char k)))]
      (lib.tty/clear-screen)
      (lib.tty/p s')
      (when-not (lib.tty/quit-alias? k')
        (recur s')))))

(defn -main []
  (try
    (lib.tty/start!)
    (print-screen-loop)
    (lib.tty/exit!)
    (catch Exception e
      (lib.tty/exit!)
      (println e))
    (finally
      (lib.tty/exit!))))

(when (= *file* (System/getProperty "babashka.file"))
  (-main))
