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
          ];
          config = (builtins.readFile ./xmonad.hs) + (
            let bindings = ""; in
            ''
              myKeybindings conf@(XConfig { XMonad.modMask = modMask }) =
                M.fromList $ [
                  ${bindings}
                ]
            ''
          );
        };
      };
    };

    home.configFile."xmobar/xmobarrc".source = ./xmobar.hs;
  };
}
