{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
{
  imports = [
    ./hardware-configuration.nix
  ];

  user.packages = with pkgs; [ ];

  services.jellyfin.enable = true;
  networking.firewall = {
    allowedTCPPorts = [ 8096 ];
    allowedUDPPorts = [ 8096 ];
  };
  user.extraGroups = [ "jellyfin" ];

  services.openssh.enable = true;

  user.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDibt+8wZVFphnJzRJtPGMpcL4JffPfMuraO8x/YOVXcqH5pymXfAxAWblF9KyGfv/bZuFoPNEGbnDlseCNvu0gsUwGO0iDahRffPq2IjLCUVxf/7+5sBYyQ64NqiS/JqVNLyhTugR+LEN+hhQgHvY4/HiR7bFpDTHkJO90Vd8DKoTsING3CPQEowO9gMpQ5+6cJk13sj6R1kD36rwd/YAhrV0SmlXKQXvzqb34oEHJkZjMmctmLkcNhmKcbIQDvm7Mj2FulUjwIjTSQNlJySuwB1JWZ50RqbedZh9JExqWyS+ZwmVdNy2K+5UA80jD7cUWP8kGLT7RRpIgEJ4IK+m+STat8vsRzivzCaOXO2i8vt2bPb9UCNOC0XOIIU2o4AtnmhhVmT3wKGy/jfvp+JcgnHZR9+Kr5XTTAmZijuz/1cToZcI4tVNlytapmoSaxEiwDPls2eZlxmrOkFXYEdCPops7G/PD/GbBd0+luJZ3pSRw7mC8rTJdjL3uh1cmDnE= floscr"
  ];

  hardware.pulseaudio.enable = true;

  modules = {
    shell = {
      git.enable = true;
      zsh.enable = true;
    };
    scripts = { };
    shared = { };
    dev = { };
    editors = {
      default = "nvim";
      vim.enable = true;
    };
    services = { };
  };
}
