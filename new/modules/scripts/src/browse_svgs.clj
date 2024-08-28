#!/usr/bin/env bb

(ns browse-svgs
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [babashka.process :refer [shell]]))

(defn svg-files [dir]
  (filter #(.endsWith (.getName %) ".svg")
          (file-seq (io/file dir))))

(defn read-svg-content [file]
  (slurp file))

(defn generate-html [svg-files]
  (str "<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>SVG Icon Grid</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 0; padding: 20px; background-color: #f0f0f0; }
        h1 { text-align: center; color: #333; }
        .grid-container {
            display: grid;
            grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
            gap: 20px;
            padding: 20px;
        }
        .icon-container {
            background-color: white;
            border-radius: 8px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
            padding: 10px;
            display: flex;
            flex-direction: column;
            align-items: center;
            transition: transform 0.2s;
        }
        .icon-container:hover {
            transform: scale(1.05);
        }
        .svg-preview {
            width: 100%;
            height: 100px;
            display: flex;
            align-items: center;
            justify-content: center;
            margin-bottom: 10px;
        }
        .svg-preview svg {
            max-width: 100%;
            max-height: 100%;
            width: auto;
            height: auto;
        }
        .filename {
            font-size: 12px;
            color: #666;
            text-align: center;
            word-break: break-all;
        }
    </style>
</head>
<body>
    <h1>SVG Icon Grid</h1>
    <div class=\"grid-container\">
    " (str/join "\n" (map (fn [file]
                            (let [filename (.getName file)]
                              (str "<div class=\"icon-container\">
        <div class=\"svg-preview\">"
                                   (read-svg-content file)
                                   "</div>
        <div class=\"filename\">" filename "</div>
    </div>")))
                          svg-files)) "
    </div>
</body>
</html>"))

(defn open-in-browser [file-path]
  (try
    (shell "xdg-open" file-path)
    (println "Opened in default browser.")
    (catch Exception e
      (println "Failed to open in browser. Please open the file manually.")
      (println "Error:" (.getMessage e)))))

(defn create-temp-file []
  (let [temp-file (java.io.File/createTempFile "svg-grid-" ".html")]
    (.deleteOnExit temp-file)
    temp-file))

(defn -main [& args]
  (if-let [dir (first args)]
    (let [svg-files (svg-files dir)
          html-content (generate-html svg-files)
          temp-file (create-temp-file)
          output-path (.getAbsolutePath temp-file)]
      (spit temp-file html-content)
      (println "Icon grid generated:" output-path)
      (open-in-browser output-path))
    (println "Usage: ./browse-svgs.clj <directory>")))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

(comment
  (-main "/home/floscr/Code/Work/Hyma/tokens-studio-for-penpot/")
  nil)
