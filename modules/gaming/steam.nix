{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.gaming.steam;
in
{
  options.modules.desktop.gaming.steam = with types; {
    enable = mkBoolOpt false;
    hardware.enable = mkBoolOpt false;
    libDir = mkOpt str "$XDG_DATA_HOME/steamlib";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # I avoid programs.steam.enable because it installs another steam binary,
      # which the xdesktop package invokes, instead of my steam shims below.
      hardware.opengl.enable = true;
      hardware.opengl.driSupport32Bit = true;
      hardware.pulseaudio.support32Bit = config.hardware.pulseaudio.enable;

      user.packages = with pkgs; [
        # Get steam to keep its garbage out of $HOME
        (writeScriptBin "steam" ''
          #!${stdenv.shell}
          # For some reason this file prevents steam from launching
          rm -rf ~/.local/share/Steam/bootstrap.tar.xz;
          HOME="${cfg.libDir}" exec ${unstable.steam}/bin/steam "$@"
        '')
        # for running GOG and humble bundle games
        (writeScriptBin "steam-run" ''
          #!${stdenv.shell}
          # For some reason this file prevents steam from launching
          rm -rf ~/.local/share/Steam/bootstrap.tar.xz;

          HOME="${cfg.libDir}" exec ${unstable.steam-run-native}/bin/steam-run "$@"
        '')
        # (makeDesktopItem {
        #   name = "steam";
        #   desktopName = "Steam";
        #   icon = "steam";
        #   exec = "steam";
        #   terminal = "false";
        #   mimeTypes = ["x-scheme-handler/steam"];
        #   categories = ["Network" "FileTransfer" "Game"];
        # })
      ];
      system.userActivationScripts.setupSteamDir = ''mkdir -p "${cfg.libDir}"'';

      modules.bindings.items = [
        {
          description = "Binding of Isaac";
          command = "steam steam://rungameid/250900";
        }
        {
          description = "FTL: Faster Than Light";
          command = "steam steam://rungameid/212680";
        }
        {
          description = "Into The Breach";
          command = "steam steam://rungameid/590380";
        }
      ];

      # better for steam proton games
      systemd.extraConfig = "DefaultLimitNOFILE=1048576";
    }

    (mkIf cfg.hardware.enable {
      hardware.steam-hardware.enable = true;
    })
  ]);
}
