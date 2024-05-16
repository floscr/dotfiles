(ns lib.acf-parser
  (:require
   [clojure.string :as str]
   [instaparse.core :as insta]))

(defn strip-quotes [s]
  (subs s 1 (dec (count s))))

;; Inspired by https://wildh4ckademistappeared.wordpress.com/2015/03/10/json-parser-in-19-lines-of-code/
(def parser
  (insta/parser "
ACF = KEY_VALUE_PAIR
<VALUE> = STRING|OBJECT
<WHITESPACE> = <#'\\s+'>
KEY_VALUE_PAIR = STRING WHITESPACE* VALUE
OBJECT = CURLY_OPEN WHITESPACE* ?[KEY_VALUE_PAIR WHITESPACE* (WHITESPACE* KEY_VALUE_PAIR WHITESPACE*)*] CURLY_CLOSE
<CURLY_OPEN> = <'{'>
<CURLY_CLOSE> = <'}'>
STRING = #'\"[^\"]+\"'
"))

(def transformer
  {:KEY_VALUE_PAIR (fn [k v] {(strip-quotes (second k)) (case (first v)
                                                          :STRING (strip-quotes (second v))
                                                          :OBJECT (into {} (rest v))
                                                          v)})})

(defn parse [s]
  (->> (str/trim s)
       (parser)
       (second)
       (insta/transform transformer)))

(comment
  (-> (slurp "/home/floscr/.config/dotfiles/new/modules/scripts/src/lib/acf_example.acf")
      (parse))
  nil)
