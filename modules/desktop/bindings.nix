{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
{
  config = mkIf config.modules.bindings.enable {
    modules.bindings.items = (mkMerge [
      (mkIf config.modules.desktop.bspwm.enable [
        {
          binding = "super + BackSpace";
          command = "zzz";
          description = "Turn off display";
        }
        {
          binding = "super + shift + BackSpace";
          command = "zzz -f";
          description = "Sleep";
        }
        {
          command = "caffeine";
          description = "Caffeine";
        }
        {
          binding = "super + grave";
          command = "bspc node -f last";
          description = "Switch to previous window";
        }
        {
          binding = "super + comma";
          command = "nimx scratchTerminal";
          description = "Switch to previous window";
        }
        {
          binding = "super + Tab";
          command = "bspc desktop -f last";
          description = "Switch to previous desktop";
        }
        {
          binding = "super + w";
          command = "bspc node --close";
          description = "Close window";
        }
        {
          binding = "super + shift + w";
          command = "bspc node --kill";
          description = "Kill process";
        }
        {
          binding = "super + {_,shift +}{1-9,0}";
          command = "bspc {desktop -f,node -d} \^{1-9,10}";
          description = "Switch to desktop";
        }
        {
          binding = "super + {h,j,k,l}";
          command = "${binDir}/bspc/focus {west,south,north,east}";
          description = "Focus/Swap the node in the given direction";
        }
        {
          binding = "super + shift + {h,j,k,l}";
          command = "${binDir}/bspc/swap {west,south,north,east}";
          description = "Focus/Swap the node in the given direction";
        }
        {
          binding = "super + {_,ctrl + }f";
          command = "bspc node -t ~{floating,fullscreen}";
          description = "Toggle floating/fullscreen";
        }
        {
          binding = "super + shift + apostrophe";
          command = "bspc node -g sticky";
          description = "Toggle sticky";
        }
        {
          binding = "super + shift + f";
          command = "${binDir}/bspc/toggle_desktop_mode";
          description = "Toggle Desktop Mode";
        }
        {
          binding = "super + ctrl + {_,shift + }v";
          command = "bspc node @/ --flip {vertical,horizontal}";
          description = "Flip Splits";
        }
        {
          binding = "super + alt + b";
          command = "bspc node @brother -B";
          description = "Flip Splits";
        }
        {
          binding = "super + ctrl + {h,j,k,l}";
          description = "Resize Window";
          command = "${binDir}/bspc/resize {west,south,north,east}";
        }
      ])
      (mkIf config.modules.desktop.bspwm.enable (
        let bsp-layout = "$HOME/.config/dotfiles/bin/bsp-layout/src/layout.sh"; in
        [
          {
            description = "Layout: Tall";
            command = "${bsp-layout} set tall";
          }
          {
            description = "Layout: Reverse Tall";
            command = "${bsp-layout} set rtall";
          }
          {
            description = "Layout: Wide";
            command = "${bsp-layout} set wide";
          }
          {
            description = "Layout: Reverse Wide";
            command = "${bsp-layout} set rwide";
          }
          {
            description = "Layout: Grid";
            command = "${bsp-layout} set grid";
          }
          {
            description = "Layout: Reverse Grid";
            command = "${bsp-layout} set rgrid";
          }
          {
            description = "Layout: Balance Windows";
            command = "${bsp-layout} set even";
          }
          {
            description = "Layout: Default (tiles)";
            command = "${bsp-layout} set tiled";
          }
          {
            description = "Layout: Reset";
            command = "${bsp-layout} remove";
          }
          {
            description = "Layout: Cycle";
            command = "${bsp-layout} cycle";
          }
        ]
      ))
      (mkIf config.services.xserver.enable [
        ## Screenshots
        {
          binding = "super + shift + S";
          command = "nimx screenCapture";
          description = "Screenshot";
        }
        {
          binding = "super + ctrl + s";
          command = "scrrec -s ~/Media/Screenrecording/$(date +%F-%T).mp4";
          description = "Record Screen (MP4)";
        }
        {
          binding = "super + ctrl + alt + s";
          command = "scrrec -s -w ~/Media/Screenrecording/$(date +%F-%T).mp4";
          description = "Record Screen (MP4) + Screenkey";
        }
        {
          binding = "super + alt + s";
          command = "scrrec -s ~/Media/Screenrecording/$(date +%F-%T).gif";
          description = "Record Screen (GIF)";
        }
        {
          binding = "super + u";
          command = "xcolor-yank";
          description = "Xcolor";
        }
        {
          command = "dragon_downloads";
          description = "Dragon: Downloads";
        }
        {
          binding = "super + Return";
          command = "termite";
          description = "New Terminal";
        }
        {
          binding = "super + t";
          command = "toggle-polybar";
          description = "Toggle Polybar";
        }
        {
          binding = "XF86Bluetooth";
          command = "bluetooth-toggle";
          description = "Toggle buetooth";
        }
        {
          binding = "super + shift + x";
          command = "org-capture-frame";
          description = "Emacs Org Capture";
        }
        {
          binding = "super + Escape";
          command = ''pkill -USR1 -x sxhkd; notify-send "Reloaded shortcuts"'';
          description = "Reload Shortcuts";
        }
        {
          binding = "super + XF86MonBrightnessDown";
          command = "light -S 0.01";
          description = "Screen brightness: Minimum";
        }
        {
          binding = "super + XF86MonBrightnessUp";
          command = "light -S 100";
          description = "Screen brightness: Maximum";
        }
        {
          binding = "XF86MonBrightnessUp";
          command = "light -A 5";
          description = "Screen brightness: -5%";
        }
        {
          binding = "{ XF86AudioLowerVolume, super + alt + j }";
          command = "amixer -q set Master 10%- unmute";
          description = "Volume: -10%";
        }
        {
          binding = "XF86MonBrightnessDown";
          command = "light -U 5";
          description = "Screen brightness: Decrease 5%";
        }
        {
          binding = "{ XF86AudioRaiseVolume, super + alt + k }";
          command = "amixer -q set Master 10%+ unmute";
          description = "Volume: +10%";
        }
        {
          binding = "{ XF86AudioPlay, super + alt + p }";
          command = "playerctl play-pause";
          description = "Toggle Play Pause";
        }
        {
          binding = "XF86MonBrightnessDown";
          command = "light -U 5";
          description = "Screen brightness: Decrease 5%";
        }
      ])
      [
        {
          command = "sudo ${pkgs.linuxPackages.cpupower}/bin/cpupower frequency-set -g performance";
          description = "CPUPower: Performance";
        }
        {
          command = "sudo ${pkgs.linuxPackages.cpupower}/bin/cpupower frequency-set -g powersave";
          description = "CPUPower: Powersave";
        }
      ]
    ]);
  };
}
