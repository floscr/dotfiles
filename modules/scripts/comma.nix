{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.scripts.comma;
  script-name = "comma";
in
{
  options.modules.scripts.comma = {
    enable = mkBoolOpt false;
  };

  config = {
    user.packages = with pkgs; [
      (writeShellScriptBin "," ''
        set -euo pipefail
        usage() {
          >&2 echo "Usage: , <package> <executable> [-- <args>...]"
        }
        if [ $# -lt 1 ]; then
          usage; exit 1
        fi
        if [[ "$1" =~ ^.*#(.*)$ ]]; then
          package="$1"
          if [ $# -lt 2 ] || [ "$2" = "--" ]; then
            executable="''${BASH_REMATCH[1]}"
          else
            executable="$2"
          fi
        else
          package="nixpkgs#$1"
          if [ $# -lt 2 ] || [ "$2" = "--" ]; then
            executable="$1"
          else
            executable="$2"
          fi
        fi
        shift
        [ $# -gt 0 ] && shift
        [ $# -gt 0 ] && [ "$1" = "--" ] && shift
        nix shell "$package" -c "$executable" "$@"
      '')
    ];
  };
}
