{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.xmonad;
in
{
	options.modules.desktop.xmonad = {
		enable = mkBoolOpt false;
	};

	config = mkIf cfg.enable {
		services = {
			xserver = {
				enable = true;
				displayManager.defaultSession = "none+xmonad";
				windowManager.xmonad = {
					enable = true;
					enableContribAndExtras = true;
					extraPackages = pkgs: with pkgs; [ dbus ];
					# config = ./xmonad.hs;
				};
			};
		};
	};
}
