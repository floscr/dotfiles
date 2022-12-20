{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modules.desktop.gtk;
in
{
  options.modules.desktop.gtk = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # Emacs keybindings for gkt applications
    home.configFile."gtk-3.0/gtk.css".source = ./gtk-emacs-bindings.css;

    services.xserver.displayManager.sessionCommands = ''
      # GTK2_RC_FILES must be available to the display manager.
      export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
    '';

    # Try really hard to get QT to respect my GTK theme.
    env.GTK_DATA_PREFIX = [ "${config.system.path}" ];
    env.QT_QPA_PLATFORMTHEME = "gtk2";
    qt5 = { style = "gtk2"; platformTheme = "gtk2"; };
  };
}
