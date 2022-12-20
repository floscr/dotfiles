{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.browsers.brave;
in
{
  options.modules.desktop.browsers.brave = with types; {
    enable = mkBoolOpt false;
    profiles = mkOption {
      type = listOf (submodule ({ name, ... }: {
        options.dir = mkOption {
          description = "The directory name for the profile.";
          type = str;
          default = "";
        };
        options.alias = mkOption {
          description = "The name of the profile by which it should be called by.";
          type = str;
          default = "";
        };
      }));
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      brave
    ];
    modules.bindings.items = [
      {
        description = "Brave";
        command = "brave";
      }
      {
        description = "Brave (Private)";
        command = "brave --incognito";
      }
    ] ++ (map
      (x: {
        description = "Brave: ${x.alias}";
        command = "brave --profile-directory=\"${x.dir}\"";
      })
      cfg.profiles);

    home-manager.users.${config.user.name}.programs.brave = {
      enable = true;
      extensions = [
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
        "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium
        "eimadpbcbfnmbkopoojfekhnkhdbieeh" # Dark Readerr
        "fipfgiejfpcdacpjepkohdlnjonchnal" # Keyboard shortcuts
        "gbmdgpbipfallnflgajpaliibnhdgobh" # Json Viewer
        "icpgjfneehieebagbmdbhnlpiopdcmna" # New Tab Redirect
        "kbmfpngjjgdllneeigpgjifpgocmfgmb" # Reddit Enhancement Suite
        "lhaoghhllmiaaagaffababmkdllgfcmc" # Atomic Chrome
        "ndiaggkadcioihmhghipjmgfeamgjeoi" # Add URL To Window Title
        "kkkjlfejijcjgjllecmnejhogpbcigdc" # Org Protocol Capture
      ];
    };
  };
}
