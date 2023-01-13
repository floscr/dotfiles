#!/usr/bin/env bb
(ns hey
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :as bp]
            [clojure.string :as str]
            [cheshire.core :as json]
            [clojure.java.io :as io]))

;; Variables -------------------------------------------------------------------

(def opts {:dir (-> (fs/expand-home "~/.config/dotfiles") (str))})

;; Helpers ---------------------------------------------------------------------

(defmacro bold [str]
  `(str "\033[1m" ~str "\033[0m"))

;; Commands --------------------------------------------------------------------

(defn rebuild! [{:keys [opts]}]
  (let [{:keys [command]
         :or [command "switch"]} opts
        hostname (-> (bp/shell {:out :string} "hostname")
                     :out)]
    (bp/shell
     {:dir (:dir opts)
      :continue true}
     (format "sudo nixos-rebuild --flake .#%s %s --impure"
             hostname
             command))))

(defn search! [{:keys [opts]}]
  (let [{:keys [query]} opts
        pkgs (-> (bp/shell {:out :string} "nix search nixpkgs" query "--json" "--quiet")
                 (:out)
                 (json/parse-string keyword))]
    (doseq [{:keys [pname description]} (vals pkgs)]
      (println (bold pname) "\n" (if (str/blank? description)
                                   ""
                                   (str description "\n"))))
    pkgs))

(defn help [{:keys []}]
  (println "Help"))

;; Main ------------------------------------------------------------------------

(def table
  [{:cmds ["re"] :fn rebuild!}
   {:cmds ["rebuild"] :fn rebuild!}
   {:cmds ["s"] :args->opts [:query] :fn search!}
   {:cmds ["search"] :fn search!}
   {:cmds [] :fn help}])

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

;; Testing ---------------------------------------------------------------------

(comment

  (def q (search! {:opts {:query "cowsay"}}))
  (vals q)
  (-main "re"))
