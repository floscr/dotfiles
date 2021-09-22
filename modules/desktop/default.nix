{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop;
in
{
  config = mkIf config.services.xserver.enable {
    assertions = [
      {
        assertion = (countAttrs (n: v: n == "enable" && value) cfg) < 2;
        message = "Can't have more than one desktop environment enabled at a time";
      }
      {
        assertion =
          let srv = config.services;
          in
          srv.xserver.enable ||
          srv.sway.enable ||
          !(anyAttrs
            (n: v: isAttrs v &&
            anyAttrs (n: v: isAttrs v && v.enable))
            cfg);
        message = "Can't enable a desktop app without a desktop environment";
      }
    ];

    user.packages = with pkgs; [
      xclip
      feh
      xdotool
      (pkgs.writeScriptBin "dragon_downloads" ''
        #!${stdenv.shell}
        cd ~/Downloads
        ls -t | head -n 15 | xargs -d '\n' bash -c '${dragon-drop}/bin/dragon --and-exit "$@"'
      '')
      (pkgs.writeScriptBin "xcolor-yank" ''
        #!${stdenv.shell}
        xcolor | tr -d '\n' | xclip -selection clipboard -in
      '')
    ];

    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      fontconfig = {
        enable = true;
        dpi = 180;
        defaultFonts = with config.modules.theme.fonts; {
          monospace = [ mono.family ];
          sansSerif = [ sans.family ];
          serif = [ serif.family ];
        };
        useEmbeddedBitmaps = true;
      };
      fonts =
        with pkgs; [
          fira-code
          fira-code-symbols
          iosevka
          roboto
          roboto-mono
          source-code-pro
          source-sans-pro
          source-serif-pro
          ubuntu_font_family
          ibm-plex

          # Unicode and Symbols
          font-awesome-ttf
          noto-fonts
          noto-fonts-cjk
          symbola
        ] ++ (mapAttrsToList (_: v: v.pkg) config.modules.theme.fonts);
    };

    # Try really hard to get QT to respect my GTK theme.
    env.GTK_DATA_PREFIX = [ "${config.system.path}" ];
    env.QT_QPA_PLATFORMTHEME = "gtk2";
    qt5 = { style = "gtk2"; platformTheme = "gtk2"; };

    services.xserver.displayManager.sessionCommands = ''
      # GTK2_RC_FILES must be available to the display manager.
      export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
    '';

    # Clean up leftovers, as much as we can
    system.userActivationScripts.cleanupHome = ''
      pushd "${homeDir}"
      rm -rf .compose-cache .nv .pki .dbus .fehbg
      [ -s .xsession-errors ] || rm -f .xsession-errors*
      popd
    '';
  };
}
