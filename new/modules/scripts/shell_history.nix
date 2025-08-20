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
          content = ./src/shell_history.clj;
        });
        cmd = "${pkg}/bin/shell_history";
      in
      {
        modules.shell.zsh.rcInit = ''
          # Shell history logging
          preexec() {
            if [[ -n "$1" && "$1" != "shell_history"* ]]; then
              ${cmd} log "$1" &
            fi
          }
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
