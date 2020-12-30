{ config, lib, pkgs, ... }:

with lib;
{
  # imports =
  #   [ home-manager.nixosModules.home-manager ];

  # Configure nix and nixpkgs
  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
  };
}
