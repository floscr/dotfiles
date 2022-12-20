{ options, config, pkgs, lib, ... }:
with lib;
let
  cfg = config.modules.desktop.xmonad;
in
{
  options.modules.desktop.xmonad = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    modules.desktop.gtk.enable = true;

    user.packages = with pkgs; [
      xmobar
      # TODO: Fix xmonadctl it doesn't work correctly right now because config dir is not here
      # (pkgs.writers.writeHaskellBin
      #   "xmonadctl"
      #   {
      #     libraries = [
      #       pkgs.haskellPackages.xmonad
      #       pkgs.haskellPackages.xmonad-extras
      #       pkgs.haskellPackages.xmonad-contrib
      #     ];
      #   }
      #   "${config.dotfiles.configDir}/xmonad/xmonadctl.hs"
      # )
    ];

    services = {
      xserver = {
        enable = true;
        displayManager.defaultSession = "none+xmonad";
        windowManager.xmonad = {
          enable = true;
          enableContribAndExtras = true;
          extraPackages = pkgs: with pkgs; [
            dbus
            haskell-language-server
          ];
          config = (builtins.readFile "${config.dotfiles.configDir}/xmonad/xmonad.hs") + (
            let
              bindings = (fold
                (cur: acc: if isNull cur.xmonadBinding then acc else ''${acc} , ("${cur.xmonadBinding}", spawn "${cur.command}")'') ""
                config.modules.bindings.items);
            in
            (
              ''
                myNixKeys :: [(String, X ())]
                myNixKeys = [("M-C-r", spawn "xmonad --recompile") ${bindings}]
              ''
            )
          );
        };
      };
    };

    home.configFile."xmobar/xmobarrc".source = "${config.dotfiles.configDir}/xmonad/xmobar.hs";
  };
}
