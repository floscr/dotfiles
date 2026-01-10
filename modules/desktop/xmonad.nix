{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.xmonad;
in
{
  options.modules.desktop.xmonad = {
    enable = mkBoolOpt false;
    showBattery = mkBoolOpt true;
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

    home.configFile."xmobar/xmobarrc".text = let
      baseConfig = builtins.readFile "${configDir}/xmonad/xmobar.hs";
      batteryCommands = ''
                , Run Battery [
                        "--template" , "<acstatus>"
                        , "--Low"      , "25"
                        , "--High"     , "50"
                        , "--"
                        --battery specific options
                        -- discharging
                        , "-o" , "<leftipat> <left>%"
                        -- AC
                        , "-O" , "<leftipat> <left>%"
                        , "-i" , "<leftipat> <left>%"
                        , "--off-icon-pattern"  , ""
                        , "--lows"              , ""
                        , "--mediums"           , ""
                        , "--highs"             , ""
                        , "--on-icon-pattern"   , ""
                        , "--idle-icon-pattern" , ""
                        , "-A" , "5"
                        , "-a" , "dunstify -u critical -a Battery \"Battery Low\" \"Your computer will turn of soon\" > /tmp/battery_notification_id"
                ] 50
      '';
      configWithoutBattery = builtins.replaceStrings
        [ batteryCommands "%battery%" ]
        [ "" "" ]
        baseConfig;
    in
      if cfg.showBattery then baseConfig else configWithoutBattery;
  };
}
