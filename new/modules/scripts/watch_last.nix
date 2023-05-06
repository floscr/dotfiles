{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.watch_last;
in
{
  options.modules-new.scripts.watch_last = with my; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable
    (
      let
        pkg = (pkgs.writeBb "watch_last" {
          content = ./src/watch_last.clj;
        });
      in
      {
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

            vendorSha256 = "sha256-T989LzKmv3KftJkbcdIMvmV72EjpY7Ddxc55OCpI7sM=";

            nativeBuildInputs = [ ];
          })
        ];
        modules.bindings.items = [
          {
            description = "Reopen last watched";
            command = "watch_last";
          }
        ];
      }

    );
}
