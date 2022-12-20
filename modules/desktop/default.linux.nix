{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.modules.desktop;
  inherit (pkgs.stdenv.targetPlatform) isLinux;
in
{
  config = mkIf cfg.enable
    (optionalAttrs isLinux {

      user.packages = with pkgs; [
        xclip
        feh
        xdotool
        (pkgs.writeScriptBin "dragon_downloads" ''
          #!${stdenv.shell}
          cd ~/Downloads
          ls -t | head -n 15 | xargs -d '\n' bash -c '${xdragon}/bin/dragon --and-exit "$@"' _
        '')
        (
          let
            xcolor = "${pkgs.xcolor}/bin/xcolor";
            xclip = "${pkgs.xclip}/bin/xclip";
          in
          pkgs.writeScriptBin "xcolor-yank" ''
            #!${stdenv.shell}
            ${xcolor} | tr -d '\n' | ${xclip} -selection clipboard -in
          ''
        )
      ];

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
