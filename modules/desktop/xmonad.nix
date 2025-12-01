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
      xcwd
      xmobar
      (pkgs.writers.writeHaskellBin
        "xmonadctl"
        {
          libraries = [
            pkgs.haskellPackages.xmonad
            pkgs.haskellPackages.xmonad-extras
            pkgs.haskellPackages.xmonad-contrib
          ];
        }
        ./xmonadctl.hs
      )
    ];

    services = {
      displayManager.defaultSession = "none+xmonad";
      xserver = {
        enable = true;
        windowManager.xmonad = {
          enable = true;
          enableContribAndExtras = true;
          extraPackages = pkgs: with pkgs; [
            dbus
            haskell-language-server
          ];
          config = (builtins.readFile "${configDir}/xmonad/xmonad.hs") + (
            let
              bindings = (foldr
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

    home.configFile."xmobar/xmobarrc".source = "${configDir}/xmonad/xmobar.hs";
  };
}
