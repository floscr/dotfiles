(ns template
  (:require
   [babashka.cli :as cli]
   [babashka.http-client :as http]
   [cheshire.core :as json]
   [clojure.string :as str]))

;; Config ----------------------------------------------------------------------

(def endpoint "https://api.openai.com/v1/chat/completions")

(def token (-> (or (System/getenv "OPENAI_TOKEN_FILE")
                   "/run/agenix/openai")
               slurp
               (str/trim)))

(def response
  (http/post
    "https://api.openai.com/v1/chat/completions"
    {:headers {"Content-Type" "application/json"
               "Authorization" (str "Bearer " token)}
     :body (-> {:model "gpt-3.5-turbo"
                :messages [{:role "user", :content "Hello!"}]
                :max_tokens 600}
               (json/generate-string))}))

;; Helpers ---------------------------------------------------------------------

;; ;; Read the user input
;; (def input
;;   (let [input-str (slurp *in*)]
;;     (clojure.string/trim input-str)))

;; ;; Define the request data
;; (def data
;;   {:prompt input
;;    :max-tokens 2048})

;; ;; Define the request headers
;; (def headers
;;   {"Content-Type" "application/json"
;;    "Authorization" (str "Bearer " token)})

;; ;; Make the HTTP request to the OpenAI API
;; (try
;;   (let [response (http/post endpoint
;;                     {:body (json/write-str data)
;;                      :headers headers})]
;;     (println (json/read-str (:body response))))
;;   (catch Exception e
;;     (println "Error making API request:" e)))

;; Commands --------------------------------------------------------------------

(defn main [args]
  (print (slurp token)))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds [] :fn main}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

(comment
  (-main)
  (-main "sub")
  nil)
