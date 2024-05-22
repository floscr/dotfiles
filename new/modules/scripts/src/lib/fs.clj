(ns lib.fs
  (:require [babashka.fs :as fs]))

(defn current-directory
  "Returns the current execution directory.
  For the repl this will be the path where the repl has been launched."
  []
  (System/getProperty "user.dir"))

(defn find-parent-with
  "Traverse updward from a `path` until `pred` function returns true."
  [pred path]
  (cond
    (pred path) path
    (fs/parent path) (find-parent-with pred (fs/parent path))
    :else nil))

(defn find-git-root
  "Find git root directory for `path`.
  Returns nil when not found.
  Ignores worktree roots."
  [path]
  (find-parent-with #(fs/directory? (fs/path % ".git")) path))

(defn find-git-worktree-root
  "Find either the git worktree root or the git root directory for `path`.
  Returns nil when not found."
  [path]
  (find-parent-with #(or (fs/regular-file? (fs/path % ".git"))
                         (fs/directory? (fs/path % ".git")))
                    path))

(defn expand
  "Wrapper around `babashka.fs/expand-home`.
  Directly returns the string."
  [path]
  (-> path fs/expand-home str))

(defn rename-extension [path new-ext]
  (let [[file _] (fs/split-ext path)]
    (if (fs/directory? path)
      path
      (fs/path (str file "." new-ext)))))

(defn fs-temp-file-path
  "Create a temp file path without the file."
  [& opts]
  (let [file (fs/create-temp-file opts)]
    (fs/delete file)
    (str file)))

(comment
  (rename-extension "/" "JPG")
  (rename-extension "/lol" "JPG")
  (rename-extension "foo.png" "JPG")
  (rename-extension "foo" "png")
  (rename-extension "foo." "png")
  nil)
