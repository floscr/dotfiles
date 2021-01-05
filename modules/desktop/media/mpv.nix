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
        terminal = false;
        name = "notube";
        desktopName = "notube";
        mimeType = "x-scheme-handler/notube";
        exec = "${notube}/bin/notube %u";
        type = "Application";
      })

      (mpv-with-scripts.override {
        scripts = [
          pkgs.mpvScripts.mpris # playerctl support
          # (fetchurl {
          #   # peerflix-hook
          #   url = "https://gist.githubusercontent.com/floscr/004f4b4d840a6ee0be40328744525c74/raw/903a183827d943abd4a914d0666a337f4e403f9c/peerflix-hook.lua";
          #   sha256 = "945c32353f2ee16b4838f9384e8428fff9705dcfb3838ac03b4dab45c58ceef0";
          # })
        ];
      })

      (mkIf config.services.xserver.enable
        celluloid)  # nice GTK GUI for mpv
    ];
  };
}
