{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.xmonad;
in
{
  options.modules.desktop.xmonad = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {

    user.packages = with pkgs; [
      xmobar
      haskellPackages.brittany
      haskellPackages.stylish-haskell
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
          config = (builtins.readFile ./xmonad.hs) + (
            let bindings = (fold
              (cur: acc: if isNull cur.xmonadBinding then acc else ''${acc} , ("${cur.xmonadBinding}", spawn "${cur.command}")'') ""
              config.modules.bindings.items); in
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

    home.configFile."xmobar/xmobarrc".source = ./xmobar.hs;
  };
}
