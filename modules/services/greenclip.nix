{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.greenclip;
in
{
  options.modules.services.greenclip = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (
    let greenclip = pkgs.unstable.haskellPackages.greenclip;
    in
    {
      user.packages = with pkgs; [
        greenclip
        xorg.libXdmcp
        (
          let
            rofi = "${pkgs.rofi}/bin/rofi";
            xdotool = "${pkgs.xdotool}/bin/xdotool";
            xclip = "${pkgs.xclip}/bin/xclip";
          in
          pkgs.writeShellScriptBin "rofi-greenclip" ''
            #!${pkgs.stdenv.shell}
            ${rofi} -modi "clipboard:greenclip print" -show clipboard -run-command '{cmd}'
            if [[ "$1" == "type" ]]; then
                sleep 0.5
                ${xdotool} type "$(${xclip} -o -selection clipboard)"
            fi
          ''
        )
      ];
      modules.bindings.items = [
        {
          binding = "super + shift + v";
          command = "rofi-greenclip";
          description = "Greenclip";
          categories = "Clipboard Manager";
        }
        {
          binding = "super + alt + shift + v";
          command = "rofi-greenclip type";
          description = "Greenclip (Type selection)";
          categories = "Clipboard Manager";
        }
      ];
      systemd.user.services.greenclip = {
        enable = true;
        description = "greenclip daemon";
        wantedBy = [ "graphical-session.target" ];
        after = [ "graphical-session.target" ];
        serviceConfig = {
          ExecStart = "${greenclip}/bin/greenclip daemon";
        };
      };
    }
  );
}
