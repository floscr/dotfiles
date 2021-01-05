{ config, options, lib, pkgs, home-manager, ... }:

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
    home-manager.users.${config.user.name}.xdg = {
      mimeApps.defaultApplications = {
        "audio/x-mp3" = [ "mpv.desktop" ];
      };
      configFile = {
        # Copy url with time stamp of web videos
        # "mpv/scripts/copy-timestamp.lua".source = ./scripts/input-copy-timestamped-url.lua;
        "mpv/scripts/lib-copy-paste.lua".source = ./scripts/lib-copy-paste.lua;
        "mpv/scripts/lib-web-video.lua".source = ./scripts/lib-web-video.lua;
        # Set youtube quality via Ctrl+f
        "mpv/scripts/youtube-quality.lua".source = ./scripts/youtube-quality.lua;
        # Reload video (when stuck while buffering) with Ctrl+r
        "mpv/scripts/reload.lua".source = ./scripts/reload.lua;

        # Keep log of videos played
        "mpv/scripts/keep-history-log.lua".source = ./scripts/keep-history-log.lua;

        # Catch up audio to video lag
        "mpv/scripts/autospeed.lua".source = ./scripts/autospeed.lua;

        # Automatically try to load next file
        "mpv/scripts/autoload.lua".source = ./scripts/autoload.lua;

        "mpv/input.conf".text = ''
            ### Mouse Bindings
            WHEEL_UP      add volume 2
            WHEEL_DOWN    add volume -2
            WHEEL_LEFT    add volume -2
            WHEEL_RIGHT   add volume 2

            ### Keyboard Bindings

            l seek  5
            L seek  60
            h seek -5
            H seek -60

            Ctrl+h playlist-prev
            Ctrl+l playlist-next

            BS revert-seek
            Ctrl+0 seek 0 absolute-percent
            Ctrl+1 seek 10 absolute-percent
            Ctrl+2 seek 20 absolute-percent
            Ctrl+3 seek 30 absolute-percent
            Ctrl+4 seek 40 absolute-percent
            Ctrl+5 seek 50 absolute-percent
            Ctrl+6 seek 60 absolute-percent
            Ctrl+7 seek 70 absolute-percent
            Ctrl+8 seek 80 absolute-percent
            Ctrl+9 seek 90 absolute-percent

            k add volume 5
            j add volume -5
            K add volume 10
            J add volume -10

            > multiply speed 11:10  # Extra speed
            < multiply speed 9:10   # v speed
            0 set speed 1.0         # Controlled speed

            a cycle audio
            s cycle sub

            ### Video Modifications
            + add video-zoom 0.5
            - add video-zoom -0.5; script-message reset-pan-if-visible
            = no-osd set video-zoom 0; script-message reset-pan-if-visible

            # Toggle Pixel Interpolation
            # a cycle-values scale nearest ewa_lanczossharp

            # Toggle color management on or off
            c cycle icc-profile-auto

            # Screenshot of the window output
            S no-osd screenshot video

            # Toggle aspect ratio information on and off
            A cycle-values video-aspect "-1" "no"

            # playlist view
            g script-message playlist-view-toggle

            # Copy time-stamped
            Ctrl+y script-binding copy-timestamped-url
        '';

        "mpv/mpv.conf".text = ''
            # MPV config

            # Every possible settings are explained here:
            # https://github.com/mpv-player/mpv/tree/master/DOCS/man

            ## VIDEO

            hwdec=auto-safe
            vo=gpu
            profile=gpu-hq

            # X11
            # x11-bypass-compositor=yes
            # demuxer-thread=yes

            # The default profile you use for your stuff. Always use this one
            # profile=gpu-hq

            # The called API. Vulkan is highly recommended.
            # Use "opengl" if you have compatibility problems
            # gpu-api=vulkan

            # The backend with the API. Leave it "auto"
            # Or use "winvk" with "gpu-api=vulkan" or "win" / "angle" with "gpu-api=opengl"
            # gpu-context=x11vk

            # Choose the compiler for translating glsl code for Vulkan. Leave it "auto"
            # Or use "shaderc" with a nVidia/AMD/Intel GPU or "nvidia" with a nVidia GPU
            # spirv-compiler=auto

            # Scaling method
            # ewa_lanczossharp is the most processor heavy, but also the prettiest
            # scale=ewa_lanczossharp
            # cscale=ewa_lanczossoft
            # # scale-radius=3
            # fbo-format=rgba16f

            # Reduce stuttering caused by mismatches in the video fps
            video-sync=display-resample
            interpolation
            tscale=oversample

            # Sync up video with audio
            autosync=30
            framedrop=vo # Skip some frames to maintain A/V sync on slow systems
            ontop=yes # Keep the player window on top of all other windows.

            # Reduce banding
            deband-iterations=2
            deband-range=12
            temporal-dither=yes

            # Load the embedded ICC  profile contained in media files such  as PNG  images.
            icc-profile-auto=yes
            icc-cache-dir=/home/${config.user.name}/.cache/mpv-icc

            ## UI

            script-opts=osc-layout=box

            # Show UI when seeking
            osd-on-seek=bar

            cursor-autohide-fs-only
            cursor-autohide=1000

            keep-open=yes
            save-position-on-quit
            stop-screensaver=yes

            # Start centered window
            geometry=50%:50%
            autofit-larger=60%x60%
            autofit-smaller=10%x10%

            ## CLI OUTPUT

            msg-module       # prepend module name to log messages
            msg-color        # color log messages on terminal
            use-filedir-conf # look for additional config files in the directory of the opened file                        # 'auto' does not imply interlacing-detection

            ## SCREENSHOTS

            screenshot-format=png
            screenshot-png-compression=0
            screenshot-png-filter=0
            screenshot-tag-colorspace=yes
            screenshot-high-bit-depth=yes
            screenshot-directory=/home/${config.user.name}/Media/Pictures/Screenshots

            ## SUBTITLES

            demuxer-mkv-subtitle-preroll              # try to correctly show embedded subs when seeking
            sub-auto=fuzzy                            # external subs don't have to match the file name exactly to autoload
            sub-file-paths=ass:srt:sub:subs:subtitles # search for external subs in the listed subdirectories
            embeddedfonts=yes                         # use embedded fonts for SSA/ASS subs
            sub-fix-timing=no                         # do not try to fix gaps (which might make it worse in some cases)
            sub-ass-force-style=Kerning=yes           # allows you to override style parameters of ASS scripts

            sub-scale-by-window=yes
            sub-font-size=45
            sub-color="#ffffffff"
            sub-border-color="#000000"
            sub-border-size=3.0
            sub-shadow-offset=0.5
            sub-shadow-color="#000000"

            # Change subtitle encoding. For Arabic subtitles use 'cp1256'.
            # If the file seems to be valid UTF-8, prefer UTF-8.
            sub-codepage=utf8

            # Languages
            slang=en,eng,enm,de,deu,ger # automatically select these subtitles (decreasing priority)
            alang=en,eng,de,deu,ger     # automatically select these audio tracks (decreasing priority)

            ## YOUTUBE-DL / STREAMING VIDEO

            # Use this user agent for streaming
            user-agent = "Mozilla/5.0"

            # Use max bitrate for HLS streaming
            hls-bitrate=max

            ytdl=yes
            ytdl-format=bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best

            # protocol config
            [protocol.http]
            force-window=immediate
            [protocol.https]
            force-window=immediate
            [protocol.ytdl]
            force-window=immediate
            profile=protocol.http

            ## AUDIO

            alsa-resample=no
            audio-channels=2
            af=format=channels=2
            audio-pitch-correction=yes
            audio-display=no

            [audio]
            force-window=no
            no-video
            ytdl-format=bestaudio/best

            ## IMAGES

            [extension.webm]
            loop-file=inf
            [extension.gif]
            loop-file=inf
            [extension.jpeg]
            loop-file=inf
            [extension.png]
            loop-file=inf
            [extension.jpg]
            loop-file=inf
            [extension.gifv]
            loop-file=inf
        '';

      };
    };
  };
}
