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
    user.packages = [
      # Fork of youtube-dl that deals with throttling of downloads
      pkgs.unstable.yt-dlp
    ];
    environment.shellAliases = {
      youtube-dl = "yt-dlp";
      youtube-dl-audio = "youtube-dl -x --audio-format vorbis --prefer-ffmpeg";
    };
  };
}
