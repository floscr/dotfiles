{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.work.meisterlabs;
in
{
  options.modules.work.meisterlabs = {
    enable = mkBoolOpt false;
  };

  config = mkIf (cfg.enable) (mkMerge [
    # Security
    (
      let
        clamavUser = "clamav";
        stateDir = "/var/lib/clamav";
        runDir = "/run/clamav";
        clamavGroup = clamavUser;
        cfg = config.services.clamav;
        pkg = pkgs.clamav;
        toKeyValue = lib.generators.toKeyValue {
          mkKeyValue = lib.generators.mkKeyValueDefault { } " ";
          listsAsDuplicateKeys = true;
        };

        freshclamConfigFile = pkgs.writeText "freshclam.conf" (toKeyValue cfg.updater.settings);
      in
      {
        environment.systemPackages = with pkgs; [
          pkg
        ];

        users.users.${clamavUser} = {
          uid = config.ids.uids.clamav;
          group = clamavGroup;
          description = "ClamAV daemon user";
          home = stateDir;
        };

        users.groups.${clamavGroup} =
          { gid = config.ids.gids.clamav; };

        environment.etc."clamav/clamd.conf".source = pkgs.writeText "clamd.conf"
          (toKeyValue {
            DatabaseDirectory = stateDir;
            LocalSocket = "${runDir}/clamd.ctl";
            PidFile = "${runDir}/clamd.pid";
            TemporaryDirectory = "/tmp";
            User = "clamav";
            Foreground = true;
          });

        environment.etc."clamav/freshclam.conf".source = pkgs.writeText "clamd.conf"
          (toKeyValue {
            DatabaseDirectory = stateDir;
            Foreground = true;
            Checks = cfg.updater.frequency;
            DatabaseMirror = [ "database.clamav.net" ];
          });

        systemd.services.av-update = {
          enable = true;
          description = "ClamAV virus database updater (freshclam)";

          preStart = ''
            mkdir -m 0755 -p ${stateDir}
            chown ${clamavUser}:${clamavGroup} ${stateDir}
          '';

          serviceConfig = {
            Type = "oneshot";
            ExecStart = "${pkg}/bin/freshclam";
            SuccessExitStatus = "1"; # if databases are up to date
            PrivateTmp = "yes";
            PrivateDevices = "yes";
          };
        };

        systemd.services.av-scan = {
          enable = true;
          description = "ClamAV daemon (clamd)";

          preStart = ''
            mkdir -m 0755 -p ${runDir}
            chown ${clamavUser}:${clamavGroup} ${runDir}
          '';

          serviceConfig = {
            Type = "oneshot";
            ExecStart = "${pkg}/bin/clamd";
            ExecReload = "${pkgs.coreutils}/bin/kill -USR2 $MAINPID";
            PrivateTmp = "yes";
            PrivateDevices = "yes";
            PrivateNetwork = "yes";
          };
        };


      }
    )
    # Syslock
    {
      networking.firewall.enable = true;
      services.physlock = {
        enable = true;
        allowAnyUser = true;
        lockOn = {
          suspend = false;
        };

      };
      modules.bindings.items = [
        {
          description = "Lock X11";
          categories = "Work";
          command = ''physlock'';
        }
      ];
    }

    # Default Config
    {
      user.packages = with pkgs; [
        ruby
        xdragon
        (writeShellScriptBin "mm-build-bundle" (builtins.readFile ./bin/mm-build-bundle))
      ];

      modules.shell.zsh.rcFiles = [ ./env.zsh ];

      modules.bindings.items = [
        {
          description = "MM: Start";
          categories = "Work";
          command = ''cd ~/Code/Meisterlabs/docker-dev-environment; ./mindmeister/restart; notify-send "MM Docker Started"'';
        }
        {
          command = "${pkgs.xdragon}/bin/dragon --and-exit ~/Code/Meisterlabs/test-data/**/*.*";
          description = "Dragon: Work DnD Test Data";
        }
      ];
    }
  ]);
}
