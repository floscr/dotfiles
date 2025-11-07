{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.shell_history;
in
{
  options.modules-new.scripts.shell_history = with my; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable
    (
      let
        pkg = (pkgs.writeBb "shell_history" {
          content = "${config.user.home}/.config/dotfiles/new/modules/scripts/src/shell_history.clj";
          useSourcePath = true;
        });
        cmd = "${pkg}/bin/shell_history";
      in
      {
        modules.shell.zsh.rcInit = ''
          preexec() {
            if [[ -n "$1" && "$1" != "shell_history"* ]]; then
              ${cmd} log "$1" </dev/null >/dev/null 2>&1 &!
            fi
          }
          
          # # Function to select command from history with fzf
          # shell-history-fzf() {
          #   local selected_command
          #   selected_command=$(${cmd} fzf | fzf --height=40% --reverse --prompt="Command: ")
          #   if [[ -n "$selected_command" ]]; then
          #     BUFFER="$selected_command"
          #     CURSOR=$#BUFFER
          #     zle redisplay
          #   fi
          # }

          # # Function to select command from history with interactive prompt
          # shell-history-interactive() {
          #   local selected_command
          #   selected_command=$(${cmd} interactive)
          #   if [[ -n "$selected_command" ]]; then
          #     BUFFER="$selected_command"
          #     CURSOR=$#BUFFER
          #     zle redisplay
          #   fi
          # }

          # # Create zle widgets and bind keys
          # zle -N shell-history-fzf
          # zle -N shell-history-interactive
          # bindkey '^R' shell-history-interactive
          # bindkey '^F' shell-history-fzf
        '';
        user.packages = with pkgs; [
          pkg
          (pkgs.buildGoModule rec {
            name = "pod-babashka-go-sqlite3";
            pname = "pod-babashka-go-sqlite3";

            src = pkgs.fetchFromGitHub {
              owner = "babashka";
              repo = "pod-babashka-go-sqlite3";
              rev = "dcc850d";
              sha256 = "sha256-Oq0BZ7700IP/yo5XZm6+iKAwuwu3Z0sMZ+9G4VBl8DQ=";
            };

            vendorHash = "sha256-T989LzKmv3KftJkbcdIMvmV72EjpY7Ddxc55OCpI7sM=";

            nativeBuildInputs = [ ];
          })
        ];
      }

    );
}
