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

(defn diff! []
  (when-let [[a b] (->> (bp/shell {:out :string} "ls -v /nix/var/nix/profiles")
                        :out
                        (#(str/split % #"\n"))
                        (take-last 2)
                        (map #(str "/nix/var/nix/profiles/" %)))]
    (bp/shell {:continue true} "nvd diff" a b)))

(defn rebuild! [{:keys [opts]}]
  (let [{:keys [command diff? dir]
         :or {command "switch"
              diff? true}} opts
        hostname (-> (bp/shell {:out :string} "hostname")
                     :out)]
    (bp/shell
     {:dir dir
      :continue true}
     (format "sudo nixos-rebuild --flake .#%s %s --impure"
             hostname
             command))
    (when diff? (diff!))))

(bp/shell {:out :string
           :dir "/tmp"} "pwd")

(defn update! [{:keys [_opts]}]
  (println "Updating NixOS flake")
  (bp/shell {:dir (:dir opts)
             :out :string} "sudo nix flake update" (System/getenv "DOTFILES")))

(defn upgrade! [args]
  (println "Upgrading NixOS flake")
  (update! args)
  (rebuild! args))

(defn rollback! [args]
  (let [arg (assoc-in args [:opts :command] "--rollback switch")]
    (rebuild! arg)))

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
  {:rebuild         {:fn rebuild!
                     :description "Rebuild current system flake"}
   :search          {:fn search!
                     :description "Searches nixpkgs for a package"
                     :args->opts [:query]}
   :garbage-collect {:fn garbage-collect!
                     :description "Garbage collect and optimize store"}
   :rollback        {:fn rollback!
                     :description "Roll back to last generation"}
   :update          {:fn update!
                     :description "Update flake lockfile"}
   :upgrade         {:fn upgrade!
                     :description "Update flake lockfile and rebuild"}
   :help            {:fn help}})

(def table
  (->> [{:cmds ["re"]              :template :rebuild}
        {:cmds ["rebuild"]         :template :rebuild}
        {:cmds ["u"]               :template :update}
        {:cmds ["update"]          :template :update}
        {:cmds ["up"]              :template :upgrade}
        {:cmds ["upgrade"]         :template :upgrade}
        {:cmds ["s"]               :template :search}
        {:cmds ["search"]          :template :search}
        {:cmds ["gc"]              :template :garbage-collect}
        {:cmds ["garbage-collect"] :template :garbage-collect}
        {:cmds ["rollback"]        :template :rollback}
        {:cmds []                  :template :help}]
       (map (fn [x] (merge x (get table-templates (:template x)))))))

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

;; Testing ---------------------------------------------------------------------

(comment

  (def q (search! {:opts {:query "cowsay"}}))
  (vals q)
  (-main "re"))
