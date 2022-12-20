{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.modules.desktop;
  inherit (pkgs.stdenv.targetPlatform) isLinux;
in
{
  config = mkIf cfg.enable
    (optionalAttrs isLinux {
      fonts = {
        fontDir.enable = true;
        enableGhostscriptFonts = true;
        fontconfig = {
          enable = true;
          defaultFonts = with config.modules.theme.fonts; {
            emoji = [ "Noto Color Emoji" "EmojiOne Color" ];
            monospace = [ mono.family ];
            sansSerif = [ sans.family ];
            serif = [ serif.family ];
          };
        };

        fonts =
          with pkgs; [
            fira-code
            fira-code-symbols
            unstable.iosevka
            roboto
            roboto-mono
            source-code-pro
            source-sans-pro
            source-serif-pro
            ubuntu_font_family
            ibm-plex

            # Unicode and Symbols
            font-awesome
            noto-fonts
            noto-fonts-cjk
            symbola
          ] ++ (mapAttrsToList (_: v: v.pkg) config.modules.theme.fonts);
      };
    });
}
