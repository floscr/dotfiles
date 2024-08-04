(ns lib.x11
  (:require
   [babashka.process :as bp]
   [lib.shell :refer [sh]]
   [clojure.string :as str]))

(defn display-resolution []
  (when-let [[width height] (some->> (sh "xrandr")
                                     (str/split-lines)
                                     (filter #(re-find #"\*" %))
                                     (first)
                                     (str/triml)
                                     (#(str/split % #" "))
                                     (first)
                                     (#(str/split % #"x"))
                                     (map parse-long)
                                     (into []))]
    {:width width
     :height height}))

(comment
  (display-resolution)
  nil)

(defn match-window-info-line [line]
  (let [[_ k _ v] (re-find #"(.+?)(:?\s+)(.*)" line)
        key (-> k
                (str/lower-case)
                (keyword))]
    {key v}))

(comment
  (match-window-info-line "Corners:  +0+0  -0+0  -0-0  +0-0")
  (match-window-info-line "-geometry 3840x2160+0+0")
  nil)

(defn window-info
  ([] (window-info "-root"))
  ([id]
   (let [info-map
         (->> (bp/sh ["xwininfo" id])
              :out
              (str/split-lines)
              (eduction
               (filter #(str/starts-with? % " "))
               (map str/trim)
               (map match-window-info-line))
              (into {}))]
     (-> info-map
         (update :width #(Integer/parseInt %))
         (update :height #(Integer/parseInt %))
         (update :depth #(Integer/parseInt %))))))

(comment
  (window-info)
  nil)
