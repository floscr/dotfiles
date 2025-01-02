#!/usr/bin/env bb

(require '[babashka.curl :as curl]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(def api-key "YOUR_API_KEY_HERE")

;; Pure babashka-friendly date handling
(def now
  (-> (java.time.Instant/now)
      (.toString)
      (subs 0 10)))

(defn days-since [date-str]
  (let [video-date (subs date-str 0 10)
        cmd (format "date -d '%s' +%%s" video-date)
        video-epoch (-> (shell/sh "bash" "-c" cmd) :out str/trim Long/parseLong)
        current-epoch (-> (shell/sh "date" "+%s") :out str/trim Long/parseLong)]
    (int (/ (- current-epoch video-epoch) 86400))))

(defn fetch-channel-videos [channel-id]
  (println "🔍 Fetching videos for channel:" channel-id)
  (-> (str "https://www.googleapis.com/youtube/v3/search"
           "?key=" api-key
           "&channelId=" channel-id
           "&part=snippet"
           "&order=viewCount"
           "&maxResults=50"
           "&type=video")
      curl/get
      :body
      (json/parse-string true)
      :items))

(defn fetch-video-stats [video-ids]
  (println "📊 Fetching stats for" (count video-ids) "videos")
  (-> (str "https://www.googleapis.com/youtube/v3/videos"
           "?key=" api-key
           "&id=" (str/join "," video-ids)
           "&part=statistics")
      curl/get
      :body
      (json/parse-string true)
      :items))

(defn calculate-score [views days-old]
  (* (Math/log10 (Double/parseDouble views))
     (Math/pow 0.995 days-old)))

(defn format-views [views]
  (str/replace views #"\B(?=(\d{3})+(?!\d))" ","))

(defn extract-video-id [video]
  (get-in video [:id :videoId]))

(defn merge-video-with-stats [stats video]
  (let [video-id (extract-video-id video)
        video-stats (first (filter #(= (:id %) video-id) stats))
        views (get-in video-stats [:statistics :viewCount] "0")
        days-old (days-since (get-in video [:snippet :publishedAt]))
        score (calculate-score views days-old)]
    {:title (get-in video [:snippet :title])
     :url (str "https://youtube.com/watch?v=" video-id)
     :views (format-views views)
     :days_old days-old
     :score score}))

(defn process-channel [channel-id]
  (->> (fetch-channel-videos channel-id)
       ((juxt identity
              (comp fetch-video-stats
                    (partial map extract-video-id))))
       (apply map merge-video-with-stats)
       (sort-by :score >)
       (take 10)
       (map #(dissoc % :score))))

(defn format-video [{:keys [title url views days_old]}]
  (format "📺 %s\n   %s\n   👀 %s views • %d days old\n"
          title url views days_old))

(defn -main [& args]
  (if-let [channel-id (first args)]
    (->> (process-channel channel-id)
         (map format-video)
         (cons "🎥 Top videos by popularity and freshness:\n\n")
         (run! println))
    (println "Usage: ./yt-ranker.clj CHANNEL_ID")))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
