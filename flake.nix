{
  description = "A grossly incandescent nixos config.";

  inputs =
    {
      nixpkgs.url = "nixpkgs/nixos-unstable";
      nixpkgs-unstable.url = "nixpkgs/master";
      nixpkgs-virtualbox.url = "github:nixos/nixpkgs/e754546ef7c3a7d5890f17dab7e6d03db16c1e1f";
      nixos-hardware.url = "github:nixos/nixos-hardware";

      home-manager.url = "github:rycee/home-manager/master";
      home-manager.inputs.nixpkgs.follows = "nixpkgs";

      secrets = { url = "/etc/dotfiles-private"; flake = false; };

      emacs-overlay.url = "github:nix-community/emacs-overlay/5fb664ca10830b0d1552b478b957091493ba2904";
      nur.url = "github:nix-community/NUR";

      flake-utils.url = "github:ursi/flake-utils/d939d2e5d73cd3468a05661e4471838b64547e6b";
      org_print_scan.url = "github:floscr/org_print_scan";
    };

  outputs = inputs @ {
      home-manager,
      nixpkgs,
      nixpkgs-unstable,
      nixpkgs-virtualbox,
      nur,
      org_print_scan,
      secrets,
      self,
      flake-utils,
      ...
  }:
    let
      inherit (lib) attrValues;
      inherit (lib.my) mapModules mapModulesRec mapHosts;

      system = "x86_64-linux";

      mkPkgs = pkgs: extraOverlays: import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = extraOverlays ++ (attrValues self.overlays) ++ [(_: super:
          {
            flake-packages = flake-utils.defaultPackages system
              { inherit org_print_scan; };
          }
        )];
      };
      pkgs = mkPkgs nixpkgs [ self.overlay nur.overlay ];
      uPkgs = mkPkgs nixpkgs-unstable [];
      vPkgs = mkPkgs nixpkgs-virtualbox [];

      lib = nixpkgs.lib.extend
        (self: super: { my = import ./lib { inherit pkgs inputs; lib = self; }; });
    in {
      lib = lib.my;

      overlay =
        final: prev: {
          unstable = uPkgs;
          virtualboxPkgs = vPkgs;
          user = self.packages."${system}";
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
