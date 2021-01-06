{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    ruby
  ];
  my = {
    packages = with pkgs; [
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
    zsh.rc = lib.readFile ./env.zsh;

    bindings = [

    ];
  };
}
