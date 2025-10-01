{ pkgs, lib, ... }:

{
  # Pipewire gets set somewhere and it collides with pulseaudio
  # My setup works so I'm not ready to switch yet.
  services.pipewire.enable = lib.mkForce false;

  services.pulseaudio = {
    enable = true;
    support32Bit = true;
    package = pkgs.pulseaudioFull;
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

  # sound.extraConfig = ''
  #   # Use PulseAudio plugin hw
  #   pcm.!default {
  #     type plug
  #     slave.pcm hw
  #   }
  # '';

  environment.systemPackages = with pkgs; [
    playerctl
    pavucontrol
    blueman
    alsa-utils
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
      pactl set-source-mute 21 1
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
      description = "Player: Metadata";
      categories = "Script, Audio";
      command = "notify-send \"$(playerctl metadata)\"";
    }
    {
      binding = "{ XF86AudioPlay, super + alt + p }";
      xmonadBinding = "<XF86AudioPlay>";
      description = "Media: Play / Pause";
      command = "playerctl play-pause";
    }
    {
      xmonadBinding = "M-M1-p";
      command = "playerctl play-pause";
    }
    {
      binding = "{ XF86AudioNext, super + alt + l }";
      xmonadBinding = "<XF86AudioNext>";
      description = "Media: Next";
      command = "playerctl next";
    }
    {
      xmonadBinding = "M-M1-l";
      command = "playerctl next";
    }
    {
      binding = "{ XF86AudioPrev, super + alt + h }";
      xmonadBinding = "<XF86AudioPrev>";
      description = "Media: Previous";
      command = "playerctl previous";
    }
    {
      xmonadBinding = "M-M1-h";
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
      xmonadBinding = "<XF86AudioMute>";
      description = "Sound: Mute/Unmute";
      command = "pactl set-sink-mute @DEFAULT_SINK@ toggle";
    }
  ];
}
