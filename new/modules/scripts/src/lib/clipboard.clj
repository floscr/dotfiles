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

(defn content-type []
  (->> (shell/sh-lines ["xclip" "-selection" "clipboard" "-t" "TARGETS" "-o"])
       (keep (fn [x]
               (let [[_ image-kind] (re-find #"image/(\w+)" x)]
                 (cond
                   image-kind (keyword image-kind)
                   (= "STRING" x) :string
                   :else nil))))
       (first)))

(defn content-or-file []
  (let [kind (content-type)]
    (case kind
      :string [:string (shell/sh ["xclip" "-o" "-selection" "clipboard"])]
      (let [file (fs/create-temp-file {:prefix "clojure-clipboard" :suffix (str "." (name kind))})]
        #_(fs/create-file filename)
        (bp/sh ["xclip" "-o" "-selection" "clipboard" "-t" (str "image/" (name kind))] {:out (fs/file file)})
        [:file file]))))

(comment
  (content-or-file))

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
