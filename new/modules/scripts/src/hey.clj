#!/usr/bin/env bb
(ns hey
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [babashka.process :as bp]
   [clojure.pprint :as pprint]
   [clojure.string :as str]))

;; Variables -------------------------------------------------------------------

(def opts
  {:dir (-> (fs/expand-home "~/.config/dotfiles") (str))})

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
              diff? true
              dir (str (fs/expand-home "~/.config/dotfiles"))}} opts
        hostname (-> (bp/shell {:out :string} "hostname")
                     :out)]
    (bp/shell {:dir dir :continue true} (format "sudo nixos-rebuild --flake .#%s %s --impure"
                                                hostname
                                                command))
    (when diff? (diff!))))

(defn update! [{:keys [_opts]}]
  (println "Updating NixOS flake")
  (let [{:keys [dir]
         :or {dir (str (fs/expand-home "~/.config/dotfiles"))}} opts]
    (bp/shell {:dir dir :out :string} "sudo nix flake update")))

(defn upgrade! [args]
  (println "Upgrading NixOS flake")
  (update! args)
  (rebuild! args))

(defn rollback! [args]
  (let [arg (assoc-in args [:opts :command] "--rollback switch")]
    (rebuild! arg)))

(defn search! [{:keys [opts]}]
  (let [{:keys [query]} opts]
    (bp/shell ["nix-search" query])))

(defn shell! [{:keys [args]}]
  (bp/shell (concat ["nix-shell" "-p"] args)))

(defn garbage-collect! [{:keys []}]
  (println "Cleaning up your user profile...")
  (bp/shell opts "sudo nix-collect-garbage" "-d"))

(defn table->str [cmds-table table-templates]
  (->> (group-by :template cmds-table)
       (map (fn [[_ vs]] (map :cmds vs)))))

(defn help [cmds-table table-templates]
  (println (str *ns*))
  (println)
  (println (table->str cmds-table table-templates)))

;; Main ------------------------------------------------------------------------

(declare cmds-table)

(def table-templates
  {:rebuild         {:fn rebuild!
                     :description "Rebuild current system flake"}
   :search          {:fn search!
                     :description "Searches nixpkgs for a package"
                     :args->opts [:query]}
   :shell           {:fn shell!
                     :description "Star a nix shell with packages"}
   :garbage-collect {:fn garbage-collect!
                     :description "Garbage collect and optimize store"}
   :rollback        {:fn rollback!
                     :description "Roll back to last generation"}
   :update          {:fn update!
                     :description "Update flake lockfile"}
   :upgrade         {:fn upgrade!
                     :description "Update flake lockfile and rebuild"}
   :help            {:fn (fn [& _] (help cmds-table table-templates))}})

(def ^:private cmds-table
  [{:cmds ["rebuild"]         :template :rebuild}
   {:cmds ["re"]              :template :rebuild}
   {:cmds ["update"]          :template :update}
   {:cmds ["u"]               :template :update}
   {:cmds ["upgrade"]         :template :upgrade}
   {:cmds ["up"]              :template :upgrade}
   {:cmds ["search"]          :template :search}
   {:cmds ["s"]               :template :search}
   {:cmds ["shell"]           :template :shell}
   {:cmds ["sh"]              :template :shell}
   {:cmds ["garbage-collect"] :template :garbage-collect}
   {:cmds ["gc"]              :template :garbage-collect}
   {:cmds ["rollback"]        :template :rollback}
   {:cmds []                  :template :help}])

(def table
  (->> cmds-table
       (map (fn [x] (merge x (get table-templates (:template x)))))))

(defn -main [& args]
  (cli/dispatch table args))

(apply -main *command-line-args*)

;; Testing ---------------------------------------------------------------------

(comment
  (table->str cmds-table table-templates)

  (-main "re")
  (-main))
