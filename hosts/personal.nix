{ config, lib, ... }:

with lib;
{
  networking.networkmanager.enable = true;
  # The global useDHCP flag is deprecated, therefore explicitly set to false
  # here. Per-interface useDHCP will be mandatory in the future, so this
  # generated config replicates the default behaviour.
  networking.useDHCP = false;

  # .local domains on network
  services.avahi = {
    enable = true;
    nssmdns4 = true;
  };

  location = {
    latitude = 12.5;
    longitude = 55.88;
  };

  # Automatic timezone
  services.automatic-timezoned.enable = true;
  services.geoclue2.enable = true;

  # Fallback timezone if automatic detection fails
  time.timeZone = mkDefault "Europe/Vienna";

  users = {
    users.media = {
      name = "media";
      isSystemUser = true;
      uid = 6666;
      description = "Media user and group";
      home = "/var/empty";
      group = "media";
    };
    groups.media = {
      name = "media";
      gid = 6666;
      members = [ config.user.name "media" ];
    };
  };
}
