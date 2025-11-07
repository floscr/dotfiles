{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.hey;
in
{
  options.modules-new.scripts.hey = with my; {
    enable = mkBoolOpt true;
  };

  config =
    let
      script-name = "hey";
      scriptPath = "${config.user.home}/.config/dotfiles/new/modules/scripts/src/hey.clj";
      package = (writeBabashkaScriptBin script-name scriptPath "");
      bin = "${package}/bin/${script-name}";
    in
    {
      user.packages = with pkgs; [
        package
      ];
    };
}
