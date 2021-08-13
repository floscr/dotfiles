{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.flameshot;
in
{
  options.modules.services.flameshot = {
    enable = mkBoolOpt false;
  };

  config =
    let
      flameshot = pkgs.unstable.flameshot;
      flameshot-bin = "${flameshot}/bin/flameshot";
      service-name = "flameshot-daemon";
    in
    {
      user.packages = with pkgs; [
        unstable.flameshot
        (pkgs.writeScriptBin "flameshot-wrapped" ''
          #!${stdenv.shell}
          # Restart daemon on every screenshot, otherwise shortcuts don't work?
          ${systemd}/bin/systemctl --user restart ${service-name}.service;
          ${flameshot-bin} gui
        '')
      ];

      systemd.user.services."${service-name}" =
        {
          description = "Flameshot";
          serviceConfig = {
            ExecStart = flameshot-bin;
          };
        };

      modules.bindings.items = [
        {
          binding = "super + shift + d";
          xmonadBinding = "M-S-d";
          command = "flameshot-wrapped";
          description = "Flameshot";
        }
      ];
    };
}
