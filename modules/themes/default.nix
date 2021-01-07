{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.theme;
  inherit (builtins) elem filter listToAttrs pathExists readDir;
  inherit (lib) filterAttrs mkIf mkOption nameValuePair types;

  mkColorOption = name:
    mkOption {
      description = "${name} color of the color palette";
      type = types.str;
    };

  mkFontOption = description:
    mkOption {
      inherit description;
      type = types.submodule {
        options = {
          family = mkOption {
            description = "Font family";
            type = types.str;
          };
          size = mkOption {
            description = "Font size";
            type = types.ints.positive;
          };
          pkg = mkOption {
            description = "Package containing font family";
            type = types.package;
          };
        };
      };
    };
in {
  options.modules.theme = with types; {
    active = mkOption {
      type = nullOr str;
      default = null;
      apply = v: let theme = builtins.getEnv "THEME"; in
                 if theme != "" then theme else v;
      description = ''
        Name of the theme to enable. Can be overridden by the THEME environment
        variable. Themes can also be hot-swapped with 'hey theme $THEME'.
      '';
    };

    gtk = {
      theme = mkOpt str "";
      iconTheme = mkOpt str "";
      cursorTheme = mkOpt str "";
    };

    colorscheme = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Name of the colorscheme to apply to the config.
        The colorscheme must be defined as a nix file in modules/theme/colors.
      '';
    };

    # This option is controlled by cfg.colorscheme
    colors = mkOption {
      description = "16-color palette for theme various apps. Based on the base16 scheme.";
      readOnly = true;
      type = types.submodule {
        options = builtins.listToAttrs (map (name: nameValuePair name (mkColorOption name)) [
          "bg0"
          "bg1"
          "bg2"
          "bg3"
          "fg0"
          "fg1"
          "fg2"
          "fg3"
          "alert"
          "primary"
          "secondary"
          "tertiary"
          "quaternary"
          "quinary"
          "senary"
          "septary"
        ]);
      };
    };

    fonts = mkOption {
      description = "Fonts to use throughout various apps.";
      type = types.submodule {
        options = {
          sans = mkFontOption "Sans serif font";
          serif = mkFontOption "Serif font";
          mono = mkFontOption "Monospace font";
          ui = mkFontOption "Font to use for UI elements";
        };
      };
    };

    onReload = mkOpt (attrsOf lines) {};
  };

  config = mkMerge [
    (mkIf (cfg.colorscheme != null) {
      modules.theme.colors = cfg.colorschemes.${cfg.colorscheme};
    })
    {
      home.configFile = {
        # GTK
        "gtk-3.0/settings.ini".text = ''
          [Settings]
          ${optionalString (cfg.gtk.theme != "")
            ''gtk-theme-name=${cfg.gtk.theme}''}
          ${optionalString (cfg.gtk.iconTheme != "")
            ''gtk-icon-theme-name=${cfg.gtk.iconTheme}''}
          ${optionalString (cfg.gtk.cursorTheme != "")
            ''gtk-cursor-theme-name=${cfg.gtk.cursorTheme}''}
          gtk-fallback-icon-theme=gnome
          gtk-application-prefer-dark-theme=true

          gtk-xft-hinting=1
          gtk-xft-hintstyle=hintfull
          gtk-xft-rgba=none
        '';
        # GTK2 global theme (widget and icon theme)
        "gtk-2.0/gtkrc".text = ''
          ${optionalString (cfg.gtk.theme != "")
            ''gtk-theme-name="${cfg.gtk.theme}"''}
          ${optionalString (cfg.gtk.iconTheme != "")
            ''gtk-icon-theme-name="${cfg.gtk.iconTheme}"''}
          gtk-font-name="Sans 10"
        '';
        # QT4/5 global theme
        "Trolltech.conf".text = ''
          [Qt]
          ${optionalString (cfg.gtk.theme != "")
            ''style=${cfg.gtk.theme}''}
        '';
      };


    }
  ];
}
