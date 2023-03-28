{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
{
  config = mkIf config.modules.bindings.enable {
    modules.bindings.items = (mkMerge [
      (mkIf config.services.xserver.enable [
        {
          command = "caffeine";
          description = "Caffeine";
        }
        {
          binding = "super + u";
          xmonadBinding = "M-u";
          command = "xcolor-yank";
          description = "Xcolor";
        }
        {
          command = "dragon_downloads";
          description = "Dragon: Downloads";
        }
        {
          binding = "super + Return";
          command = config.modules.desktop.term.default;
          description = "New Terminal";
        }
        {
          binding = "XF86Bluetooth";
          command = "bluetooth-toggle";
          description = "Toggle buetooth";
        }
        {
          binding = "super + shift + x";
          xmonadBinding = "M-S-x";
          command = "org-capture-frame";
          description = "Emacs Org Capture";
        }
        {
          binding = "super + shift + x";
          xmonadBinding = "M-S-g";
          command = "emacsclient -a '' -e '(+my|scratch-popup)'";
          description = "Emacs Org Capture";
        }
        {
          binding = "super + XF86MonBrightnessDown";
          xmonadBinding = "M-<XF86MonBrightnessDown>";
          command = "light -S 0.01";
          description = "Screen brightness: Minimum";
        }
        {
          binding = "super + XF86MonBrightnessUp";
          xmonadBinding = "M-<XF86MonBrightnessUp>";
          command = "light -S 100";
          description = "Screen brightness: Maximum";
        }
        {
          binding = "XF86MonBrightnessUp";
          xmonadBinding = "<XF86MonBrightnessUp>";
          command = "light -A 5";
          description = "Screen brightness: -5%";
        }
        {
          binding = "XF86MonBrightnessDown";
          xmonadBinding = "<XF86MonBrightnessDown>";
          command = "light -U 5";
          description = "Screen brightness: Decrease 5%";
        }
        {
          binding = "{ XF86AudioLowerVolume, super + alt + j }";
          xmonadBinding = "<XF86AudioLowerVolume>";
          command = "pactl set-sink-volume @DEFAULT_SINK@ -10%";
          description = "Volume: -10%";
        }
        {
          xmonadBinding = "M-M1-j";
          command = "pactl set-sink-volume @DEFAULT_SINK@ -10%";
        }
        {
          binding = "{ XF86AudioRaiseVolume, super + alt + k }";
          xmonadBinding = "<XF86AudioRaiseVolume>";
          command = "pactl set-sink-volume @DEFAULT_SINK@ +10%";
          description = "Volume: +10%";
        }
        {
          xmonadBinding = "M-M1-k";
          command = "pactl set-sink-volume @DEFAULT_SINK@ +10%";
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
        {
          command = "${pkgs.systemd}/bin/systemctl reboot";
          description = "Reboot";
        }
      ]
    ]);
  };
}
