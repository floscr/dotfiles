---
name: bb-server
description: Build Babashka HTTP API servers using httpkit, ruuter, and hiccup. Scaffolds server projects with routing, handlers, rendering, WebSocket support, and HTMX integration.
---

# Babashka Server Skill

Build Babashka HTTP servers following proven patterns from production use. Uses httpkit for async HTTP, ruuter for routing, and hiccup for HTML rendering.

## Stack

| Library | Purpose | Require |
|---------|---------|---------|
| `org.httpkit.server` | Async HTTP server, WebSocket | `[org.httpkit.server :as srv]` |
| `ruuter.core` | Route matching + dispatch | `[ruuter.core :as ruuter]` |
| `hiccup2.core` | HTML generation from Clojure data | `[hiccup2.core :as h]` |
| `cheshire.core` | JSON encode/decode | `[cheshire.core :as json]` |

## Dependencies (bb.edn)

```clojure
{:deps {org.clojars.askonomm/ruuter {:mvn/version "1.3.4"}
        cheshire/cheshire {:mvn/version "5.13.0"}}
 :paths ["src"]}
```

Hiccup and httpkit are built into Babashka — no extra deps needed.

## Project Structure

```
src/
  <name>/
    server.clj           # Entry point, routes, -main
    server/
      config.clj         # Dynamic config vars, data loading
      render.clj         # HTTP response helpers (html, json, form parsing)
      handlers.clj       # Request handler functions
      components.clj     # Hiccup UI components (if serving HTML)
      websocket.clj      # WebSocket state + broadcast (if needed)
```

Scale up by adding handler files per domain. Scale down by keeping everything in `server.clj` for small APIs.

## Core Files

### Entry Point (`server.clj`)

```clojure
(ns <name>.server
  (:require [<name>.server.handlers :as handlers]
            [org.httpkit.server :as srv]
            [ruuter.core :as ruuter]))

(def routes
  [{:path "/"
    :method :get
    :response handlers/index-handler}
   {:path "/api/items"
    :method :get
    :response handlers/list-items}
   {:path "/api/items"
    :method :post
    :response handlers/create-item}
   {:path "/api/items/:id"
    :method :get
    :response handlers/get-item}
   {:path "/api/items/:id"
    :method :put
    :response handlers/update-item}
   {:path "/api/items/:id"
    :method :delete
    :response handlers/delete-item}])

(defn- wrap-errors [handler]
  (fn [req]
    (try
      (or (handler req)
          {:status 404 :body "Not found"})
      (catch Throwable e
        (println "ERROR" (:uri req) (ex-message e))
        {:status 500 :body (str "Error: " (ex-message e))}))))

(defn -main [& args]
  (let [port (or (some-> (first args) parse-long) 3000)]
    (srv/run-server (wrap-errors #(ruuter/route routes %)) {:port port})
    (println "Server running at" (str "http://localhost:" port "/"))))

(def cli? (= *file* (System/getProperty "babashka.file")))

(when cli?
  (apply -main *command-line-args*)
  @(promise))
```

### Render Helpers (`server/render.clj`)

```clojure
(ns <name>.server.render
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [hiccup2.core :as h])
  (:import [java.net URLDecoder]))

(defn render
  "Render hiccup to HTTP response"
  [body & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body (if (seq? body)
           (apply str (map #(if (string? %) % (str (h/html %))) body))
           (str (h/html body)))})

(defn render-json
  "Render data as JSON HTTP response"
  [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string data)})

(defn render-html
  "Render hiccup to HTML string (for WebSocket broadcast)"
  [hiccup]
  (str (h/html hiccup)))

(defn parse-form-body
  "Parse URL-encoded form body into keyword map"
  [body]
  (when body
    (->> (str/split (slurp body) #"&")
         (map #(str/split % #"=" 2))
         (map (fn [[k v]] [(keyword k) (when v (URLDecoder/decode v "UTF-8"))]))
         (into {}))))

(defn parse-json-body
  "Parse JSON request body into keyword map"
  [body]
  (when body
    (json/parse-string (slurp body) true)))
```

### Handlers (`server/handlers.clj`)

```clojure
(ns <name>.server.handlers
  (:require [<name>.server.render :as render]))

(defn index-handler [_req]
  (render/render
   [:html
    [:head [:title "My App"]]
    [:body
     [:h1 "Hello"]]]))

(defn list-items [_req]
  (render/render-json {:items []}))

(defn create-item [{:keys [body]}]
  (let [data (render/parse-json-body body)]
    ;; process data
    (render/render-json {:created data} 201)))

(defn get-item [{:keys [params]}]
  (let [id (:id params)]
    (render/render-json {:id id})))

(defn update-item [{:keys [params body]}]
  (let [id (:id params)
        data (render/parse-json-body body)]
    (render/render-json {:id id :updated data})))

(defn delete-item [{:keys [params]}]
  (let [id (:id params)]
    (render/render-json {:deleted id})))
```

## Route Definition

Ruuter routes are vectors of maps with `:path`, `:method`, and `:response`.

```clojure
{:path "/api/items/:id"    ;; :id becomes (:id (:params req))
 :method :get               ;; :get :post :put :delete
 :response handler-fn}      ;; (fn [request] -> response-map)
```

Route params arrive in `(:params req)`. Put more specific routes before catch-all patterns.

## Request Shape

```clojure
{:request-method :post
 :uri "/api/items/123"
 :params {:id "123"}           ;; route params from ruuter
 :query-string "foo=bar"       ;; raw query string
 :headers {"content-type" ...}
 :body <InputStream>}          ;; slurp to read
```

## HTMX Integration

For server-rendered HTML with dynamic updates:

```clojure
;; Button that posts and swaps content
[:button {:hx-post "/api/action"
          :hx-target "#result"
          :hx-swap "innerHTML"}
 "Do thing"]

;; Target div
[:div {:id "result"} "initial content"]
```

### Out-of-Band Swaps (update multiple targets from one response)

```clojure
;; Return a list — primary response + OOB updates
(render/render
 (list
  [:div {:id "main"} "primary response"]
  [:div {:id "counter" :hx-swap-oob "innerHTML"} "42"]
  [:div {:id "status" :hx-swap-oob "innerHTML"} "updated"]))
```

### Page Template with HTMX

```clojure
(defn page-template [title & body]
  [:html
   [:head
    [:title title]
    [:script {:src "https://unpkg.com/htmx.org@2.0.4"}]]
   [:body {:hx-ext "ws"}
    body]])
```

## WebSocket Support

httpkit has built-in WebSocket support.

```clojure
(ns <name>.server.websocket
  (:require [<name>.server.render :as render]
            [org.httpkit.server :as srv]))

(def ws-clients (atom #{}))

(defn ws-handler [req]
  (srv/as-channel req
    {:on-open (fn [ch] (swap! ws-clients conj ch))
     :on-close (fn [ch _] (swap! ws-clients disj ch))
     :on-receive (fn [_ch msg] (println "received:" msg))}))

(defn ws-broadcast! [html-str]
  (doseq [ch @ws-clients]
    (srv/send! ch html-str false)))

;; Broadcast hiccup as HTML to all clients
(defn broadcast-update! [hiccup]
  (ws-broadcast! (render/render-html hiccup)))
```

Add WebSocket route:

```clojure
{:path "/ws" :method :get :response websocket/ws-handler}
```

### Status Monitor Pattern (poll + broadcast changes)

```clojure
(def state-cache (atom {}))

(defn start-status-monitor! [render-row-fn]
  (future
    (loop []
      (try
        (let [current (fetch-current-state)
              prev @state-cache
              changed (filter #(not= (get prev (:id %)) (:state %)) current)]
          (reset! state-cache (into {} (map (juxt :id :state) current)))
          (doseq [item changed]
            (broadcast-update! (render-row-fn item))))
        (catch Exception e
          (println "Monitor error:" (ex-message e))))
      (Thread/sleep 2000)
      (recur))))
```

## JSON API Pattern

For pure JSON APIs without HTML:

```clojure
(defn api-handler [{:keys [params body]}]
  (let [data (render/parse-json-body body)]
    (render/render-json {:status "ok" :data data})))
```

## Async Operations

For long-running work, return immediately and update via WebSocket:

```clojure
(defn launch-handler [{:keys [body]}]
  (let [data (render/parse-form-body body)]
    ;; Return placeholder immediately
    (future
      (try
        (let [result (do-slow-thing data)]
          (broadcast-update! [:div {:id "result"} (str result)]))
        (catch Exception e
          (broadcast-update! [:div {:id "result" :class "error"} (ex-message e)]))))
    (render/render [:div {:id "result"} "Processing..."])))
```

## Running

```bash
# Direct
bb src/<name>/server.clj

# With custom port
bb src/<name>/server.clj 8080

# As bb task
bb server
```

## bb.edn Task

```clojure
{:tasks
 {:server {:doc "Start the server"
           :task (shell "bb src/<name>/server.clj")}}}
```

## Conventions

- Handler functions take a request map, return a response map
- Use `render/render` for HTML, `render/render-json` for JSON
- Route params via `(:params req)`, form body via `parse-form-body`, JSON via `parse-json-body`
- Error wrapper returns 500 with message on unhandled exceptions
- `@(promise)` at end of script keeps the server alive
- Use `(def cli? ...)` guard so the file works both as script and REPL
- Side-effecting functions use `!` suffix
- Keep routes in a flat vector, ordered specific-to-general
- Split into `server/handlers.clj`, `server/render.clj`, etc. as the project grows

## Checklist

1. Create `bb.edn` with ruuter + cheshire deps
2. Create `server.clj` with routes and `-main`
3. Create `server/render.clj` with response helpers
4. Create `server/handlers.clj` with route handlers
5. Add WebSocket if real-time updates are needed
6. Test: `bb src/<name>/server.clj`
