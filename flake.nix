{
  description = "A grossly incandescent nixos config.";

  inputs =
    {
      nixpkgs.url = "nixpkgs/nixos-unstable";
      nixpkgs-unstable.url = "nixpkgs/master";
      nixos-hardware.url = "github:nixos/nixos-hardware";

      home-manager.url = "github:rycee/home-manager/master";
      home-manager.inputs.nixpkgs.follows = "nixpkgs";

      secrets = { url = "/etc/dotfiles-private"; flake = false; };

      emacs-overlay.url = "github:nix-community/emacs-overlay";
      nur.url = "github:nix-community/NUR";

      flake-utils.url = "github:ursi/flake-utils/d939d2e5d73cd3468a05661e4471838b64547e6b";
      org_print_scan.url = "github:floscr/org_print_scan";
      rofi_org_bookmarks.url = "github:floscr/rofi_org_bookmarks";
      rofi_cmder.url = "github:floscr/rofi_cmder";
    };

  outputs =
    inputs @ { self
    , flake-utils
    , home-manager
    , nixpkgs
    , nixpkgs-unstable
    , nur
    , org_print_scan
    , rofi_cmder
    , rofi_org_bookmarks
    , secrets
    , ...
    }:
    let
      inherit (lib) attrValues;
      inherit (lib.my) mapModules mapModulesRec mapHosts;

      system = "x86_64-linux";

      mkPkgs = pkgs: extraOverlays: import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = extraOverlays ++ (attrValues self.overlays) ++ [
          (_: super:
            {
              flake-packages = flake-utils.defaultPackages system
                { inherit org_print_scan rofi_org_bookmarks rofi_cmder; };
            }
          )
        ];
      };
      pkgs = mkPkgs nixpkgs [ self.overlay nur.overlay ];
      uPkgs = mkPkgs nixpkgs-unstable [ ];

      lib = nixpkgs.lib.extend
        (self: super: { my = import ./lib { inherit pkgs inputs; lib = self; }; });
    in
    {
      lib = lib.my;

      overlay =
        final: prev: {
          unstable = uPkgs;
          user = self.packages."${system}";
        };

      overlays =
        mapModules ./overlays import;

      packages."${system}" =
        mapModules ./packages
          (p: pkgs.callPackage p { });

      nixosModules =
        {
          dotfiles = import ./.;
        }
        // mapModulesRec ./modules import;

      nixosConfigurations =
        mapHosts ./hosts { };
    };
}
