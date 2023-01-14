#!/usr/bin/env bb
(ns hey
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :as bp]
            [clojure.string :as str]
            [cheshire.core :as json]
            [clojure.java.io :as io]))

;; Variables -------------------------------------------------------------------

(def opts
  {:dir (-> (fs/expand-home "~/.config/dotfiles") (str))})

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

(defn garbage-collect! [{:keys []}]
  (println "Cleaning up your user profile...")
  (bp/shell opts "sudo nix-collect-garbage" "-d"))

(defn help [{:keys []}]
  (println "Help"))

;; Main ------------------------------------------------------------------------

(def table-templates
  {:rebuild {:fn rebuild!}
   :search {:args->opts [:query] :fn search!}
   :garbage-collect {:fn garbage-collect!}
   :help {:cmds [] :fn help}})

(def table
  (->> [{:cmds ["re"] :template :rebuild}
        {:cmds ["rebuild"] :template :rebuild}
        {:cmds ["s"] :template :search}
        {:cmds ["search"] :template :search}
        {:cmds ["gc"] :template :garbage-collect}
        {:cmds ["garbage-collect"] :template :garbage-collect}
        {:cmds [] :template :help}]
      (map (fn [x] (merge x (get table-templates (:template x)))))))

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

;; Testing ---------------------------------------------------------------------

(comment

  (def q (search! {:opts {:query "cowsay"}}))
  (vals q)
  (-main "re"))
