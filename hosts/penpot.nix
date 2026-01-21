{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
{


  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_15;
    ensureDatabases = [ "penpot" "penpot_test" "penpot_telemetry" ];
    ensureUsers = [{
      name = "penpot";
      ensureDBOwnership = true;
    }];

    enableTCPIP = true;
    authentication = ''
      local all all              trust
      host all             all             0.0.0.0/0               trust
      host  all all 127.0.0.1/32 trust
      host  all all ::1/128      trust
      host  all all all          trust
    '';
    settings = {
      listen_addresses = pkgs.lib.mkForce "*";
      max_connections = 50;
      shared_buffers = "256MB";
      temp_buffers = "18MB";
      work_mem = "18MB";

      dynamic_shared_memory_type = "posix";
      synchronous_commit = "off";
      wal_writer_delay = "900ms";
      max_wal_size = "1GB";
      min_wal_size = "80MB";
      full_page_writes = "off";

      log_timezone = "Europe/Madrid";
      datestyle = "iso, mdy";
      timezone = "Europe/Madrid";


      # lc_messages = "en_US.utf8";
      # lc_monetary = "en_US.utf8";
      # lc_numeric = "en_US.utf8";
      # lc_time = "en_US.utf8";
      default_text_search_config = "pg_catalog.english";
    };
  };

  networking.firewall.allowedTCPPorts = [ 80 443 5432 ];

  security.sudo.extraRules = [{
    users = [ "floscr" ];
    commands = [
      { command = "/run/current-system/sw/bin/systemctl stop postgresql"; options = [ "NOPASSWD" ]; }
      { command = "/run/current-system/sw/bin/systemctl start postgresql"; options = [ "NOPASSWD" ]; }
      { command = "/run/current-system/sw/bin/systemctl restart postgresql"; options = [ "NOPASSWD" ]; }
    ];
  }];

}
