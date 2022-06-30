{
  description = "My dotfiles";

  inputs =
    {
      nixpkgs.url = "nixpkgs/nixos-unstable";
      nixpkgs-unstable.url = "nixpkgs/master";
      nixos-hardware.url = "github:nixos/nixos-hardware";

      home-manager.url = "github:rycee/home-manager/master";
      home-manager.inputs.nixpkgs.follows = "nixpkgs";
      agenix.url = "github:ryantm/agenix";
      agenix.inputs.nixpkgs.follows = "nixpkgs";

      # secrets = { url = "/etc/dotfiles-private"; flake = false; };

      emacs-nixpkgs.url = "github:nixos/nixpkgs/a3e6348d2c68103b0c96e35b3d94c4ea0e6f9e50";
      emacs-overlay.url = "github:nix-community/emacs-overlay/80db8e4e9f25e81662a244a96029f3427fe3d5b9";
      nur.url = "github:nix-community/NUR";

      flake-utils.url = "github:ursi/flake-utils/d939d2e5d73cd3468a05661e4471838b64547e6b";
      org_print_scan.url = "github:floscr/org_print_scan";
      nim-utils.url = "github:floscr/nim-utils";
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
    , emacs-nixpkgs
    , emacs-overlay
    , rofi_cmder
    , rofi_org_bookmarks
    , nim-utils
    , ...
    }:
    let
      inherit (lib) attrValues;
      inherit (lib.my) mapModules mapModulesRec mapHosts;

      system = "x86_64-linux";

      mkExtraPkgs = pkgs: import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          (_: super:
            {
              nim-utils = {
                get_url_title = nim-utils.packages.${system}.get_url_title;
                bose_battery_level = nim-utils.packages.${system}.bose_battery_level;
              };
            }
          )
        ];
      };

      mkEmacsPkgs = pkgs: import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          emacs-overlay.overlay
        ];
      };

      mkPkgs = pkgs: extraOverlays: import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = extraOverlays ++ (attrValues self.overlays) ++ [
          emacs-overlay.overlay
        ] ++ [
          (_: super:
            {
              flake-packages = flake-utils.defaultPackages system
                {
                  inherit
                    # org_print_scan
                    rofi_org_bookmarks
                    rofi_cmder;
                };
            }
          )
        ];
      };
      pkgs = mkPkgs nixpkgs [ self.overlay nur.overlay ];
      uPkgs = mkPkgs nixpkgs-unstable [ ];
      emacsPkgs = mkEmacsPkgs emacs-nixpkgs;
      myCustomPkgs = mkExtraPkgs nixpkgs;

      lib = nixpkgs.lib.extend
        (self: super: { my = import ./lib { inherit pkgs inputs; lib = self; }; });
    in
    {
      lib = lib.my;

      overlay =
        final: prev: {
          emacsPkgs = emacsPkgs;
          custom = myCustomPkgs;
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
