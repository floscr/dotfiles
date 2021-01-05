{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.media.mpv;
    mpv-thumbs-cache = "/tmp/mpv_thumbs_cache";
    mpv-gallery-thumb-dir = "/tmp/mpv_gallery_cache";
    notube = pkgs.writeScriptBin "notube" ''
      #!${pkgs.stdenv.shell}
      ${pkgs.mpv-with-scripts}/bin/mpv "$(echo "$@" | sed "s/notube://")"
    '';
in {
  options.modules.desktop.media.mpv = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      mpvc

      # Peerflix
      socat
      nodePackages.peerflix

      # NoTube
      notube
      (makeDesktopItem {
        terminal = "False";
        name = "notube";
        desktopName = "notube";
        mimeType = "x-scheme-handler/notube";
        exec = "${notube}/bin/notube %u";
        type = "Application";
      })

      (mpv-with-scripts.override {
        scripts = [
          pkgs.mpvScripts.mpris # playerctl support
          (fetchurl {
            url = "https://gist.githubusercontent.com/ekisu/bba287693830055a6bad90081c1ad4e2/raw/65a97c59b9dcfc9de94864160124fbe5eb5f3aa3/peerflix-hook.lua";
            sha256 = "08h6wzrhrp1i1pbzzrim8rwa1bkvjxdvs7rqqsnj6s4b77rg1x48";
            passthru.scriptName = "peerflix-hook.lua";
          })
        ];
      })

      (mkIf config.services.xserver.enable
        celluloid)  # nice GTK GUI for mpv
    ];
  };
}
