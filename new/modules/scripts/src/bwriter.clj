(ns bwriter
  (:require
   [clojure.string :as str]
   [tty]
   [lib.str]))

(defn print-screen-loop []
  (loop [s ""]
    (let [k (tty/read-key)
          k' (tty/interpret-key k)
          s' (case k'
               :backspace (lib.str/drop-char s)
               (str s (char k)))]
      (tty/clear-screen)
      (tty/p s')
      (when-not (tty/quit-alias? k')
        (recur s')))))

(defn -main []
  (try
    (tty/start!)
    (print-screen-loop)
    (tty/exit!)
    (catch Exception e
      (tty/exit!)
      (println e))
    (finally
      (tty/exit!))))

(when (= *file* (System/getProperty "babashka.file"))
  (-main))
