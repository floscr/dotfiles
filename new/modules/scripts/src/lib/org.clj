(ns lib.org
  (:require
   [clojure.string :as str]))

;; Helpers ---------------------------------------------------------------------

(defn zero-pad
  "Zero Pad numbers - takes a number and the length to pad to as arguments."
  [n c]
  (format (str "%0" c "d") n))

;; Timestamp -------------------------------------------------------------------

(defn timestamp [{:keys [year month day hours minutes seconds rest]
                  :or {minutes 0}}]
  (let [date (str (zero-pad year 4) "-" (zero-pad month 2) "-" (zero-pad day 2))
        clock (when hours (->> [hours minutes seconds]
                               (filter some?)
                               (map #(zero-pad % 2))
                               (str/join ":")
                               (str " ")))]
    (str date (or (some->> rest (str " ")) clock))))

(defn inactive [ps]
  (format "[%s]" (timestamp ps)))

(defn active [ps]
  (format "<%s>" (timestamp ps)))

(defn timestamp->str [{:keys [type] :as v}]
  (case type
    :active (active v)
    :inactive (inactive v)))

(defn now-timestamp->str [ps]
  (let [now (java.time.ZonedDateTime/now)
        timestamp-props {:year (.getYear now)
                         :month (.getMonthValue now)
                         :day (.getDayOfMonth now)
                         :hours (.getHour now)
                         :minutes (.getMinute now)
                         :seconds (.getSecond now)}]
    (timestamp->str (merge ps timestamp-props))))

;; Properties ------------------------------------------------------------------

(defn properties->str [ps]
  (let [kvs (->> (for [[k v] ps]
                   (str ":" (name k) ":" (when v (str " " v))))
                 (str/join "\n"))]
    (str ":PROPERTIES:\n"
         kvs "\n"
         ":END:")))

;; Headline --------------------------------------------------------------------

(defn todo-headline
  ([heading-str] (todo-headline heading-str nil))
  ([heading-str properties]
   (str "* TODO " heading-str "\n"
        (properties->str (merge properties {:DATE_CREATED (now-timestamp->str {:type :inactive})})) "\n")))

(defn headline
  ([heading-str] (todo-headline heading-str nil))
  ([heading-str properties]
   (str "* TODO " heading-str "\n"
        (properties->str (merge properties {:DATE_CREATED (now-timestamp->str {:type :inactive})})) "\n")))
