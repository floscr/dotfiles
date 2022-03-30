{ config, options, pkgs, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.pass;
in
{
  options.modules.shell.pass = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      (pass.withExtensions (exts: [
        exts.pass-otp
        exts.pass-genphrase
        exts.pass-tomb
      ]))
      pkgs.qrencode # Generate QR code from pass
      (lib.mkIf (config.services.xserver.enable) rofi-pass)

    ];
    env.PASSWORD_STORE_DIR = "$HOME/.secrets/password-store";
    modules.bindings.items = [
      {
        description = "Pass";
        categories = "Password Manager";
        command = "rofi-pass";
      }
      {
        binding = "super + apostrophe";
        command = "rofi-pass -dmenu -theme theme/passmenu.rasi";
        description = "Password Manager";
      }
    ];
  };
}
