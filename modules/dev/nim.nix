{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.nim;
in
{
  options.modules.dev.nim = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    environment.variables.NIM_UNWRAPPED_DIR = "${pkgs.nim-unwrapped}";
    user.packages = with pkgs; [
      nim
      (pkgs.writeScriptBin "nimx" ''
        bin=$1
        src="$HOME/.config/dotfiles/bin/nimbin/src/$bin.nim"
        dst="$HOME/.config/dotfiles/bin/nimbin/dst/$bin"
        shift

        if [[ ! -f "$dst" || "$src" -nt "$dst" ]]; then
            echo "Compiling $src..."
            ${pkgs.nim}/bin/nim c -r \
                --verbosity:0 \
                --hint[Processing]:off \
                --excessiveStackTrace:on \
                -d:release \
                --out:$dst \
                $src
        fi

        $dst $@
      '')
    ];
  };
}
