(ns lib.clipboard
  (:require
   [clojure.string :as string]
   [babashka.process :as bp]
   [clojure.java.shell :as sh]))

;; Read clipboard --------------------------------------------------------------

(defn get-clip [clipboard-name]
  (-> (sh/sh "xclip" "-o" "-selection" clipboard-name)
      :out))

(defn get-all
  "Returns a list of things on available clipboards."
  []
  {:clipboard (get-clip "clipboard")
   :primary   (get-clip "primary")})
   ;; :secondary (get-clip "secondary")
   ;; :buffer-cut (get-clip "buffer-cut")


(defn values []
  (->> (get-all)
       vals
       (map string/trim)
       (remove empty?)))

(comment
  (values))

;; Write clipboard -------------------------------------------------------------

(defn set-clip [s]
  (-> (bp/process '[xclip -i -selection clipboard]
                 {:in s})
      bp/check
      :out
      slurp))

(comment
  (set-clip "hello\ngoodbye")
  (set-clip "siyanora")
  (get-clip "primary")
  (sh/sh "xclip" "-o" "-selection" "primary")
  (get-all))
