{ pkgs, ... }:

{
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    settings = {
      Headset = {
        FastConnectable = "true";
        HFP = "true";
      };
      General = {
        ControllerMode = "bredr";
        Disable = "Headset";
        AutoEnable = "true";
        AutoConnect = "true";
        MultiProfile = "multiple";
        Enable = "Source,Sink,Media,Socket";
      };
    };
  };

  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
    package = pkgs.pulseaudioFull;
    extraModules = [
      pkgs.pulseaudio-modules-bt
    ];
    extraConfig = ''
      load-module module-udev-detect tsched=0
      load-module module-bluetooth-policy
      load-module module-bluetooth-discover
      load-module module-switch-on-connect

      # Internal Microphone
      load-module alsa-source device=hw:0,0
      .ifexists module-udev-detect.so
    '';
    # https://medium.com/@gamunu/enable-high-quality-audio-on-linux-6f16f3fe7e1f
    daemon.config = {
      default-sample-format = "float32le";
      default-sample-rate = 48000;
      alternate-sample-rate = 44100;
      default-sample-channels = 2;
      default-channel-map = "front-left,front-right";
      default-fragments = 2;
      default-fragment-size-msec = 125;
      resample-method = "soxr-vhq";
      remixing-consume-lfe = "no";
      high-priority = "yes";
      nice-level = -11;
      realtime-scheduling = "yes";
      realtime-priority = 9;
      rlimit-rtprio = 9;
      daemonize = "no";
    };
  };

  sound.extraConfig = ''
    # Use PulseAudio plugin hw
    pcm.!default {
      type plug
      slave.pcm hw
    }
  '';

  nixpkgs.config = {
    packageOverrides = pkgs: {
      bluez = pkgs.bluez5;
    };
  };

  environment.systemPackages = with pkgs; [
    unstable.playerctl
    pavucontrol
    blueman
    (pkgs.writeScriptBin "switch-sound-monitor" ''
      #!${stdenv.shell}

      # Accept Input + Output from internal hardware
      pacmd set-card-profile 0 output:hdmi-stereo-extra1
    '')
    (pkgs.writeScriptBin "prepare-video-call" ''
      #!${stdenv.shell}

      # Accept Input + Output from internal hardware
      pacmd set-card-profile 0 output:analog-stereo+input:analog-stereo

      # Unmute the microphone
      amixer set Capture cap
    '')
  ];

  modules.bindings.items = [
    {
      description = "PulseAudio Volume Control";
      categories = "Volume Control";
      command = "pavucontrol";
    }
    {
      description = "Connect Swiss Box";
      categories = "Script";
      command = "bluetoothctl connect 30:23:80:C0:7D:85";
    }
    {
      description = "Connect Bluetooth Headphones";
      categories = "Script";
      command = "bose_connect";
    }
    {
      description = "Prepare video call";
      categories = "Script";
      command = "prepare-video-call";
    }
    {
      description = "Play Monitor Sound";
      categories = "Script";
      command = "switch-sound-monitor";
    }
    {
      binding = "{ super + alt + t }";
      description = "Toggle Headphone Audio Output";
      categories = "Script, Audio";
      command = "nimx toggleHeadphoneAudioOutput";
    }
    {
      description = "Player: Metadata";
      categories = "Script, Audio";
      command = "notify-send \"$(playerctl metadata)\"";
    }
    {
      binding = "{ XF86AudioPlay, super + alt + p }";
      description = "Media: Play / Pause";
      command = "playerctl play-pause";
    }
    {
      binding = "{ XF86AudioNext, super + alt + l }";
      description = "Media: Next";
      command = "playerctl next";
    }
    {
      binding = "{ XF86AudioPrev, super + alt + h }";
      description = "Media: Previous";
      command = "playerctl previous";
    }
    {
      binding = "{ shift + XF86AudioNext, super + alt + shift + l, alt + XF86AudioRaiseVolume  }";
      description = "Media: Forward 20s";
      command = "playerctl position 20+";
    }
    {
      binding = "{ shift + XF86AudioPrev, super + alt + shift + h, alt + XF86AudioLowerVolume   }";
      description = "Media: Forward 20s";
      command = "playerctl position 20-";
    }
    {
      binding = "{ XF86AudioMute, super + alt + m }";
      description = "Sound: Mute/Unmute";
      command = "amixer -q set Master toggle";
    }
  ];
}
