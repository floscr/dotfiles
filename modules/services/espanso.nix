# Textexpander alternative for linux
# https://espanso.org/
{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.espanso;
  yaml = pkgs.formats.yaml { };
in
{
  options.modules.services.espanso = {
    enable = mkBoolOpt false;
    config = mkOption {
      type = yaml.type;
      description = "Espanso configuration";
      default = {
        matches = [
          {
            trigger = ":repro";
            replace = ''Steps to reproduce
1. '';
          }
          {
            trigger = ":date";
            replace = "{{mydate}}";
            vars = [
              {
                name = "mydate";
                type = "date";
                params = {
                  format = "%m/%d/%Y";
                };
              }
            ];
          }
          {
            # Shell commands
            trigger = ":shell";
            replace = "{{output}}";
            vars = [
              {
                name = "output";
                type = "shell";
                params = {
                  cmd = "echo Hello from your shell";
                };
              }
            ];
          }
        ];
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services.espanso = {
      description = "Espanso daemon";
      path = with pkgs; [ espanso libnotify xclip ];
      serviceConfig = {
        ExecStart = "${pkgs.espanso}/bin/espanso daemon";
        Restart = "on-failure";
      };
      wantedBy = [ "default.target" ];
    };

    home.file.".config/espanso/default.yml".source = yaml.generate "default.yml" cfg.config;

    environment.systemPackages = [ pkgs.espanso ];
  };
}
