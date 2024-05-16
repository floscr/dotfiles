(ns lib.acf-parser
  (:require
   [strojure.parsesso.char :as char]
   [strojure.parsesso.parser :as p]))

(def double-quote (char/is "\""))

(def *whitespace (p/*many char/white?))

(def escaped-double-quote
  (-> (p/after (char/is "\\") double-quote)
      (p/value (constantly "\""))))

(def string-literal
  "String literal parsing that allows escaped double-quotes characters."
  (p/for [_ double-quote
          content (p/*many (p/alt (p/maybe escaped-double-quote)
                                  (p/token-not double-quote)))
          _ double-quote]
    (p/result content)))

(def string-field
  (p/after double-quote (p/*many-till p/any-token double-quote)))

(def key-value-pair
  (p/for [_ *whitespace
          k (p/value string-literal char/str*)
          _ *whitespace
          v (p/alt (p/value (p/maybe string-field) char/str*)
                   (-> (p/after (char/is "{")
                                *whitespace ;; Needed for empty records
                                (p/*many-till (p/maybe key-value-pair)
                                              (char/is "}")))
                       (p/value #(into {} %))))
          _ *whitespace]
    (p/result {k v})))

(def parse (partial p/parse key-value-pair))

(comment
  (-> (slurp "/home/floscr/.config/dotfiles/new/modules/scripts/src/lib/acf_example.acf")
      (parse))
  nil)
