{ config, lib, pkgs, ... }:

with lib;
with lib.my;
with inputs;
{
  imports =
    [ home-manager.nixosModules.home-manager ];

  environment.variables.DOTFILES = dotFilesDir;

  # Configure nix and nixpkgs
  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
    nixPath = [
      "nixpkgs=${nixpkgs}"
      "nixpkgs-unstable=${nixpkgs-unstable}"
      "nixpkgs-overlays=${dotFilesDir}/overlays"
      "home-manager=${home-manager}"
      "dotfiles=${dotFilesDir}"
    ];
    registry = {
      nixos.flake = nixpkgs;
      nixpkgs.flake = nixpkgs-unstable;
    };
    useSandbox = true;
  };
}
