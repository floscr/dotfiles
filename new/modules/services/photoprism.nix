{ config, lib, ... }:

with lib;

let
  cfg = config.modules.services.photoprism;
in
{
  options.modules.services.photoprism = {
    enable = mkEnableOption "photoprism";
    instances = mkOption {
      type = types.attrsOf (types.submodule {
        options = {
          storageDir = mkOption {
            description = "SQLite, cache, session, thumbnail and sidecar files are created in the storage folder";
          };
          originalsDir = mkOption {
            description = "Directory which contains photos and videos";
          };
          importDir = mkOption {
            description = "Folder from which files can be transferred to the originals folder in a structured way that avoids duplicates";
          };
          port = mkOption {
            type = types.port;
          };
          vhost = mkOption { };
        };
      });
    };

  };

  config = mkIf (cfg.enable) {
    # services.nginx = {
    #   enable = true;
    #   virtualHosts = mapAttrs'
    #     (instance: config: nameValuePair "${config.vhost}" {
    #       forceSSL = true;
    #       enableACME = true;
    #       locations."/" = {
    #         proxyPass = "http://127.0.0.1:${toString config.port}";
    #         proxyWebsockets = true;
    #         extraConfig =
    #           "proxy_redirect off;" +
    #           "proxy_set_header Host $host;" +
    #           "proxy_set_header X-Real-IP $remote_addr;" +
    #           "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;" +
    #           "proxy_set_header X-Forwarded-Proto $scheme;";
    #       };
    #     })
    #     cfg.instances;
    # };

    virtualisation.oci-containers = {
      containers = mapAttrs'
        (instance: config: nameValuePair "photoprism-${instance}" {
          image = "photoprism/photoprism@sha256:042afacef24270f7d055611b0526ba5b7f4f7d6f2f974d247adabd28c56dabc8";
          volumes = [
            "${config.storageDir}:/photoprism/storage"
            "${config.originalsDir}:/photoprism/originals"
            "${config.importDir}:/photoprism/import"
          ];
          ports = [ "${toString config.port}:${toString config.port}" ];
          environment = {
            PHOTOPRISM_ADMIN_PASSWORD = "changeme1234";
            PHOTOPRISM_HTTP_PORT = "${toString config.port}";
            # PHOTOPRISM_SITE_URL = "https://${config.vhost}";
          };
        })
        cfg.instances;
    };
  };
}
