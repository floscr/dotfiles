# flake.nix --- the heart of my dotfiles
#
# Author:  Henrik Lissner <henrik@lissner.net>
# URL:     https://github.com/hlissner/dotfiles
# License: MIT
#
# Welcome to ground zero. Where the whole flake gets set up and all its modules
# are loaded.

{
  description = "A grossly incandescent nixos config.";

  inputs =
    {
      # Core dependencies.
      # Two inputs so I can track them separately at different rates.
      nixpkgs.url          = "nixpkgs/master";
      nixpkgs-unstable.url = "nixpkgs/master";

      home-manager.url   = "github:rycee/home-manager/master";
      home-manager.inputs.nixpkgs.follows = "nixpkgs";

      secrets = { url = "/etc/dotfiles-private"; flake = false; };

      # Extras
      emacs-overlay.url  = "github:nix-community/emacs-overlay";
      nixos-hardware.url = "github:nixos/nixos-hardware";
      nur.url            = "github:nix-community/NUR";
    };

  outputs = inputs @ { self, nixpkgs, nixpkgs-unstable, nur, home-manager, secrets, ... }:
    let
      inherit (lib) attrValues;
      inherit (lib.my) mapModules mapModulesRec mapHosts;

      system = "x86_64-linux";

      mkPkgs = pkgs: extraOverlays: import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = extraOverlays ++ (attrValues self.overlays);
      };
      pkgs  = mkPkgs nixpkgs [ self.overlay nur.overlay ];
      uPkgs = mkPkgs nixpkgs-unstable [];

      lib = nixpkgs.lib.extend
        (self: super: { my = import ./lib { inherit pkgs inputs; lib = self; }; });
    in {
      lib = lib.my;

      overlay =
        final: prev: {
          unstable = uPkgs;
          my = self.packages."${system}";
        };

      overlays =
        mapModules ./overlays import;

      packages."${system}" =
        mapModules ./packages
          (p: pkgs.callPackage p {});

      nixosModules =
        {
          dotfiles = import ./.;
        }
        // mapModulesRec ./modules import;

      nixosConfigurations =
        mapHosts ./hosts { inherit system; };
    };
}
