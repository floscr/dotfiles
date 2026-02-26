---
name: fullstack-app
description: Build fullstack apps with Squint + Eucalypt frontend and Babashka + Ruuter backend. Scaffolds projects with reactive UI, JSON API, CORS, dev server, and Vite single-file builds.
---

# Fullstack App Skill (Squint + Eucalypt / Babashka + Ruuter)

Build fullstack apps with a ClojureScript frontend (Squint compilation + Eucalypt reactive rendering) and a Clojure backend (Babashka + httpkit + ruuter).

## Stack Overview

| Layer | Library | Purpose |
|-------|---------|---------|
| **Frontend** | `squint-cljs` | CLJS → JS compilation (minimal runtime) |
| **Frontend** | `eucalypt` | Reagent-like reactive rendering (atom, cursor, hiccup→DOM) |
| **Frontend** | `vite` | Dev server + production bundler |
| **Frontend** | `vite-plugin-singlefile` | Single HTML file output |
| **Backend** | `org.httpkit.server` | Async HTTP server |
| **Backend** | `ruuter.core` | Route matching + dispatch |
| **Backend** | `cheshire.core` | JSON encode/decode |

## Project Structure

```
app/<name>/
  main.cljs            # Frontend entry point (Squint source)
  index.html           # HTML shell
  style.css            # Styles
  squint.edn           # Squint compiler config
  package.json         # npm deps + scripts
  vite.config.js       # Vite config
  .gitignore           # node_modules/, dist/, *.mjs
  dist/                # Built output (single HTML file)

src/
  <name>/
    server.clj         # Backend entry point, routes, -main
    server/
      render.clj       # HTTP response helpers (EDN, JSON)
      handlers.clj     # Request handler functions
      paths.clj        # Path validation (if file access needed)
```

## Frontend

### squint.edn

```clojure
{:paths ["."]
 :output-dir "."
 :extension "mjs"}
```

### package.json

```json
{
  "devDependencies": {
    "concurrently": "^9.2.1",
    "eucalypt": "latest",
    "squint-cljs": "latest",
    "vite": "^7.1.7",
    "vite-plugin-singlefile": "^2.3.0"
  },
  "scripts": {
    "build": "squint compile && vite build",
    "watch:squint": "squint watch",
    "watch:vite": "vite --port 8000",
    "watch": "concurrently 'npm run watch:squint' 'npm run watch:vite'"
  }
}
```

### vite.config.js

```js
import { defineConfig } from "vite";
import { viteSingleFile } from "vite-plugin-singlefile";

export default defineConfig({
  plugins: [viteSingleFile()],
  build: {
    outDir: "dist",
  },
});
```

### index.html

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <title>App</title>
  <link rel="stylesheet" href="style.css" />
</head>
<body>
  <div id="app"></div>
  <script type="module" src="main.mjs"></script>
</body>
</html>
```

### .gitignore

```
node_modules/
dist/
*.mjs
```

### Eucalypt API

Eucalypt provides a Reagent-compatible API compiled through Squint:

```clojure
(ns main
  (:require [eucalypt :as r]))

;; Reactive atom — triggers re-render on change
(defonce state (r/atom {:count 0}))

;; Component (function returning hiccup)
(defn counter [state]
  (let [{:keys [count]} @state]
    [:div
     [:p (str "Count: " count)]
     [:button {:on-click #(swap! state update :count inc)} "+"]]))

;; Mount to DOM
(r/render [counter state] (js/document.getElementById "app"))
```

**Key Eucalypt APIs:**
- `r/atom` — reactive atom (Reagent ratom), triggers re-renders on deref+swap
- `r/cursor` — derived cursor into an atom `(r/cursor my-atom [:path :to :val])`
- `r/reaction` — computed value from atom derefs (read-only)
- `r/render` — mount component `[component & args]` into a DOM container
- `@state` / `(deref state)` — deref triggers watcher subscription
- `(swap! state f)` / `(reset! state v)` — triggers re-render of subscribed components

**Hiccup syntax:**
- Tags: `:div`, `:span`, `:button`, `:input`, etc.
- CSS shorthand: `:div.class-name`, `:div#id`, `:div.a.b#myid`
- Attributes map: `[:div {:class "foo" :style {"color" "red"} :on-click handler}]`
- Events: `:on-click`, `:on-change`, `:on-input`, `:on-submit`, `:on-key-down`, etc.
- Fragments: `[:<> child1 child2]`
- Conditional: `(when condition [:div "shown"])`
- Lists: `(map (fn [item] ^{:key (:id item)} [:li (:name item)]) items)`
- Components: `[my-component arg1 arg2]` (vector with fn as first element)

**Squint-specific patterns:**
- Keywords become strings: `:foo` → `"foo"` in JS
- Data structures are JS-native: `{}` → plain object, `[]` → array
- JS interop is seamless: `(js/fetch url)`, `(.-length arr)`, `(.method obj)`
- Use `#js {}` for JS object literals when needed
- `aget` for JS property access: `(aget obj "key")`
- `defonce` for state atoms to survive hot-reload

### Fetching from Backend

```clojure
(def API_URL "http://localhost:3000")

(defn fetch-data! [state]
  (swap! state assoc :loading? true :error nil)
  (-> (js/fetch (str API_URL "/api/data")
                #js {:method "GET"
                     :headers #js {"Content-Type" "application/json"}})
      (.then (fn [resp]
               (if (.-ok resp)
                 (.json resp)
                 (throw (js/Error. (str "HTTP " (.-status resp)))))))
      (.then (fn [data]
               (swap! state assoc :data data :loading? false)))
      (.catch (fn [err]
                (swap! state assoc :error (.-message err) :loading? false)))))
```

**POST with body:**

```clojure
(-> (js/fetch (str API_URL "/api/action")
              #js {:method "POST"
                   :headers #js {"Content-Type" "application/json"}
                   :body (js/JSON.stringify (clj->js payload))})
    (.then ...))
```

**POST with text/plain body (e.g. EDN config):**

```clojure
(-> (js/fetch (str API_URL "/api/endpoint?format=json")
              #js {:method "POST"
                   :headers #js {"Content-Type" "text/plain"}
                   :body config-string})
    (.then ...))
```

## Backend

### bb.edn deps

```clojure
{:deps {org.clojars.askonomm/ruuter {:mvn/version "1.3.4"}
        cheshire/cheshire {:mvn/version "5.13.0"}}
 :paths ["src"]}
```

Hiccup and httpkit are built into Babashka.

### Entry Point (server.clj)

```clojure
(ns <name>.server
  (:require
   [babashka.cli :as cli]
   [org.httpkit.server :as http]
   [ruuter.core :as ruuter]
   [clojure.string :as str]
   [<name>.server.render :as render]
   [<name>.server.handlers :as handlers]))

(defn parse-query-string [qs]
  (when (and qs (not (str/blank? qs)))
    (->> (str/split qs #"&")
         (map #(str/split % #"=" 2))
         (reduce (fn [m [k v]] (assoc m k (or v ""))) {}))))

(defn wrap-query-params [handler]
  (fn [req]
    (let [qp (parse-query-string (:query-string req))
          req (assoc req :params (merge (or (:params req) {}) qp))]
      (handler req))))

(defn wrap-cors [handler]
  (fn [req]
    (if (= :options (:request-method req))
      {:status 204
       :headers {"Access-Control-Allow-Origin" "*"
                 "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS"
                 "Access-Control-Allow-Headers" "Content-Type"}}
      (let [resp (handler req)]
        (update resp :headers merge {"Access-Control-Allow-Origin" "*"})))))

(defn wrap-errors [handler]
  (fn [req]
    (try
      (or (handler req)
          {:status 404 :body "Not found"})
      (catch Exception e
        (println "ERROR" (:uri req) (ex-message e))
        {:status 500 :body (str "Error: " (ex-message e))}))))

(def routes
  [{:path "/api/health"
    :method :get
    :response handlers/health-handler}
   {:path "/api/data"
    :method :get
    :response handlers/data-handler}])

(defn -main [& args]
  (let [opts (cli/parse-opts args {:coerce {:port :long} :alias {:p :port}})
        port (or (:port opts) 3000)
        handler (-> (fn [req] (ruuter/route routes req))
                    wrap-query-params
                    wrap-errors
                    wrap-cors)]
    (http/run-server handler {:port port})
    (println (str "Server running on http://localhost:" port))
    @(promise)))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
```

### Render Helpers (server/render.clj)

```clojure
(ns <name>.server.render
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.net URLDecoder]))

(defn render-json
  ([data] (render-json data 200))
  ([data status]
   {:status status
    :headers {"Content-Type" "application/json"}
    :body (json/generate-string data)}))

(defn render-edn
  ([data] (render-edn data 200))
  ([data status]
   {:status status
    :headers {"Content-Type" "application/edn"}
    :body (pr-str data)}))

(defn render-error
  ([msg] (render-error msg 400))
  ([msg status]
   {:status status
    :headers {"Content-Type" "application/json"}
    :body (json/generate-string {:error msg})}))

(defn parse-json-body [body]
  (when body (json/parse-string (slurp body) true)))

(defn parse-edn-body [body]
  (when body (edn/read-string (slurp body))))

(defn parse-form-body [body]
  (when body
    (->> (str/split (slurp body) #"&")
         (map #(str/split % #"=" 2))
         (map (fn [[k v]] [(keyword k) (when v (URLDecoder/decode v "UTF-8"))]))
         (into {}))))
```

### Handlers (server/handlers.clj)

```clojure
(ns <name>.server.handlers
  (:require [<name>.server.render :as render]))

(defn health-handler [_req]
  (render/render-json {:status "ok"}))

(defn data-handler [req]
  (let [format (get-in req [:params "format"])]
    (if (= "json" format)
      (render/render-json {:items []})
      (render/render-edn {:items []}))))
```

### Route Definition

Ruuter routes are vectors of maps:

```clojure
{:path "/api/items/:id"    ;; :id → (:id (:params req))
 :method :get               ;; :get :post :put :delete
 :response handler-fn}      ;; (fn [request] -> response-map)
```

Route params arrive in `(:params req)`. Query params are merged by `wrap-query-params`.

### Request Shape

```clojure
{:request-method :post
 :uri "/api/items/123"
 :params {:id "123" "format" "json"}  ;; route + query params merged
 :query-string "format=json"
 :headers {"content-type" "application/json"}
 :body <InputStream>}                 ;; slurp to read
```

## bb.edn Tasks

```clojure
{:tasks
 {server {:doc "Start the API server"
          :requires ([<name>.server :as server])
          :task (apply server/-main *command-line-args*)}

  <name> {:doc "Start frontend dev server"
          :task (shell {:dir "app/<name>"} "npm run watch")}}}
```

## Development

```bash
# Terminal 1: Backend
bb server --port 3000

# Terminal 2: Frontend (squint watch + vite dev server on :8000)
bb <name>
# or: cd app/<name> && npm run watch

# Build production single-file HTML
cd app/<name> && npm run build
# Output: app/<name>/dist/index.html
```

## Conventions

- Frontend state lives in `defonce` atoms to survive hot-reload
- Backend responds JSON when `?format=json`, EDN otherwise
- CORS middleware allows `*` for local dev (restrict in production)
- `wrap-query-params` merges query string into `:params`
- `wrap-errors` catches all exceptions, returns 500 with message
- Vite dev server on port 8000, backend on port 3000
- `vite-plugin-singlefile` bundles CSS+JS into one HTML file for distribution
- Use `^{:key id}` metadata on list items for efficient DOM patching

## Checklist

1. Create `app/<name>/` directory with `package.json`, `squint.edn`, `vite.config.js`, `index.html`, `.gitignore`
2. Run `npm install` in the app directory
3. Create `main.cljs` with eucalypt require, state atom, components, and `r/render` mount
4. Create `style.css` with styles
5. Create `src/<name>/server.clj` with routes, middleware, and `-main`
6. Create `src/<name>/server/render.clj` with response helpers
7. Create `src/<name>/server/handlers.clj` with route handlers
8. Add `bb.edn` tasks for server and frontend dev
9. Test: start backend (`bb server`), start frontend (`npm run watch`), verify fetch works
10. Build: `cd app/<name> && npm run build`, verify `dist/index.html`
