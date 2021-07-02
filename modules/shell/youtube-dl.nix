{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.youtube-dl;
in
{
  options.modules.shell.youtube-dl = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [ pkgs.youtube-dl ];
    environment.shellAliases = {
      youtube-dl-audio = "youtube-dl -x --audio-format vorbis --prefer-ffmpeg";
    };
  };
}
