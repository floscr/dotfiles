{ lib, config, pkgs, ... }:
with lib;
with lib.my;
let cfg = config.modules.theme;
in
{
  config = mkIf (cfg.mode == "dark") {
    modules.theme = {
      wallpaper = ./dj_nobu_dark.jpg;
      colours = import ./_dracula.nix;
      tridactyl = "base16-dracula";
      fonts = rec {
        sans = {
          family = "IBM Plex Sans";
          size = 8;
          pkg = pkgs.ibm-plex;
        };
        serif = {
          family = "Source Serif Pro";
          size = 8;
          pkg = pkgs.source-serif-pro;
        };
        mono = {
          family = "Fira Code";
          size = 8;
          pkg = pkgs.fira-code;
        };
        terminal = {
          family = "Iosevka";
          size = 10;
          pkg = pkgs.unstable.iosevka;
        };
        ui = sans;
      };
    };

    home = {
      configFile = {
        # Piggyback wallpaper change trigger to perform non-Nix managed updates.
        "wallpaper".onChange = "make -B -C /etc/dotfiles dark";

        "bat/config".text = ''--theme="Dracula"'';
      };
    };
  };
}
