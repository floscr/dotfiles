{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.keyboard;
    keymap = pkgs.writeText "keymap.xkb" ''
      xkb_keymap {
        xkb_keycodes  { include "evdev+aliases(qwerty)"	};
        xkb_types     { include "complete"	};
        xkb_compat    { include "complete"	};
          xkb_symbols   {
              include "pc+us+inet(evdev)+ctrl(nocaps)+terminate(ctrl_alt_bksp)"
              key <AC01> { [ a, A, adiaeresis, Adiaeresis ] };
              key <AC02> { [ s, S, ssharp, U03A3 ] };
              key <AD03> { [ e, E, EuroSign ] };
              key <AD09> { [ o, O, odiaeresis, Odiaeresis ] };
              key <AD07> { [ u, U, udiaeresis, Udiaeresis ] };
              include "level3(ralt_switch)"
          };
        xkb_geometry  { include "pc(pc104)"	};
      };
    '';
in {
  options.modules.hardware.keyboard = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    environment.etc."X11/keymap.xkb".source = keymap;

    environment.systemPackages = with pkgs; [
      xorg.xkbcomp
      xorg.xmodmap
    ];

    services.xserver = {
      autoRepeatDelay = 190;
      autoRepeatInterval = 30;
    };

    systemd.user.services."hotplug-keyboard" = {
      enable = true;
      description = "Load my keyboard modifications";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = false;
        ExecStart = "${pkgs.systemd}/bin/systemctl --user restart setup-keyboard";
      };
    };

    systemd.user.services."setup-keyboard" = {
      enable = true;
      description = "Load my keyboard modifications";
      after = [ "graphical-session-pre.target" ];
      partOf = [ "graphical-session-pre.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.bash}/bin/bash ${pkgs.writeScript "setup-keyboard.sh" ''
          #!${pkgs.stdenv.shell}

          # Stop previous xcape processes, otherwise xcape is launched multiple times
          # And buttons get implemented multiple times
          ${pkgs.killall}/bin/killall xcape

          # Load keyboard layout
          ${pkgs.xorg.xkbcomp}/bin/xkbcomp /etc/X11/keymap.xkb $DISPLAY

          # Capslock to control
          ${pkgs.xcape}/bin/xcape -e 'Control_L=Escape'

          # Make space Control L whenn pressed.
          spare_modifier="Hyper_L"
          ${pkgs.xorg.xmodmap}/bin/xmodmap -e "keycode 65 = $spare_modifier"
          ${pkgs.xorg.xmodmap}/bin/xmodmap -e "remove mod4 = $spare_modifier"
          ${pkgs.xorg.xmodmap}/bin/xmodmap -e "add Control = $spare_modifier"

          # Map space to an unused keycode (to keep it around for xcape to
          # use).
          ${pkgs.xorg.xmodmap}/bin/xmodmap -e "keycode any = space"

          # Finally use xcape to cause the space bar to generate a space when tapped.
          ${pkgs.xcape}/bin/xcape -e "$spare_modifier=space"

          echo "Keyboard setup done!"
        ''}";
      };
    };

    services.udev.extraRules = ''
      ACTION=="add", SUBSYSTEMS=="usb", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="6122", TAG+="systemd", ENV{SYSTEMD_USER_WANTS}+="hotplug-keyboard.service"
    '';
  };
}
