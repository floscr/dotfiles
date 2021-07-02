{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.work.meisterlabs;
in
{
  options.modules.work.meisterlabs = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      ruby
      dragon-drop
      (writeShellScriptBin "mm-build-bundle" ''
        #!/usr/bin/env zsh

        rm -rf build
        nice -n10 npm run "$1"
        notify-send "Bundle \"$1\" built."
        $MEISTERLABS_MINDMEISTER_DOCKER_DIR/mindmeister/bundle
        docker exec -it mm-rails /bin/bash -c "rm -rf /opt/mindmeister-web && mkdir -p /opt/mindmeister-web"
        docker cp build mm-rails:/opt/mindmeister-web
        cd $MEISTERLABS_MINDMEISTER_DOCKER_DIR
        ./mindmeister/rake "client:import_bundles_testing['$2']"
      '')
    ];

    modules.shell.zsh.rcFiles = [ ./env.zsh ];

    modules.bindings.items = [
      {
        description = "MM: Start";
        categories = "Work";
        command = ''cd ~/Code/Meisterlabs/docker-dev-environment; ./mindmeister/restart; notify-send "MM Docker Started"'';
      }
      {
        command = "${pkgs.dragon-drop}/bin/dragon --and-exit ~/Code/Meisterlabs/test-data/**/*.*";
        description = "Dragon: Work DnD Test Data";
      }
    ];
  };
}
