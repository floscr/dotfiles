{ options, config, lib, pkgs, home-manager, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.mpd;
in
{
  options.modules.services.mpd = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ mpc_cli ncpamixer ];

    user.extraGroups = [ "mpd" ];

    home-manager.users.${config.user.name}.programs.beets.enable = true;

    environment.variables.MPD_HOME = "$XDG_CONFIG_HOME/mpd";

    services = {
      mpd = {
        enable = true;
        musicDirectory = "/data/media/music";
        startWhenNeeded = true;
        extraConfig = ''
          input {
              plugin      "curl"
          }

          audio_output {
              type        "pulse"
              name        "My MPD PulseAudio Output"
              server      "127.0.0.1"
          }

          audio_output {
              type        "fifo"
              name        "mpd_fifo"
              path        "/tmp/mpd.fifo"
              format      "44100:16:2"
          }
        '';
      };
    };

    hardware.pulseaudio.tcp = {
      enable = true;
      anonymousClients.allowedIpRanges = [ "127.0.0.1" ];
    };
  };
}
