{
  description = "My dotfiles";

  inputs =
    {
      nixpkgs.url = "nixpkgs/nixos-unstable";
      nixpkgs-unstable.url = "nixpkgs/master";
      nixos-hardware.url = "github:nixos/nixos-hardware";

      # Package lock overrides
      fira-code-pkgs.url = "github:nixos/nixpkgs/a3e6348d2c68103b0c96e35b3d94c4ea0e6f9e50";
      utsushi-nixpkgs.url = "github:nixos/nixpkgs/fbf68b6e4a8c5f518977a3e4b1ec4828efbb8efd";

      # External packages
      home-manager.url = "github:rycee/home-manager/master";
      home-manager.inputs.nixpkgs.follows = "nixpkgs";
      agenix.url = "github:ryantm/agenix";
      agenix.inputs.nixpkgs.follows = "nixpkgs";
      nix-search-cli = {
        url = github:peterldowns/nix-search-cli;
        inputs.nixpkgs.follows = "nixpkgs";
      };

      # secrets = { url = "/etc/dotfiles-private"; flake = false; };

      emacs-overlay.url = "github:nix-community/emacs-overlay";
      emacs-nixpkgs.url = "github:nixos/nixpkgs/6cee3b5893090b0f5f0a06b4cf42ca4e60e5d222";
      nur.url = "github:nix-community/NUR";


      flake-utils.url = "github:ursi/flake-utils/d939d2e5d73cd3468a05661e4471838b64547e6b";
      org_print_scan.url = "github:floscr/org_print_scan";
      nim-utils.url = "github:floscr/nim-utils";
      rofi_org_bookmarks.url = "github:floscr/rofi_org_bookmarks";
      rofi_cmder.url = "github:floscr/rofi_cmder";
    };

  outputs =
    inputs @ { self
    , emacs-nixpkgs
    , emacs-overlay
    , fira-code-pkgs
    , flake-utils
    , home-manager
    , nim-utils
    , nix-search-cli
    , nixpkgs
    , nixpkgs-unstable
    , nur
    , org_print_scan
    , rofi_cmder
    , rofi_org_bookmarks
    , utsushi-nixpkgs
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
              pins = {
                nix-search-cli = nix-search-cli.packages.${system}.nix-search;
              };
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
      pkgs = mkPkgs nixpkgs [ self.overlay nur.overlays.default ];
      uPkgs = mkPkgs nixpkgs-unstable [ ];
      firaCodePkgs = mkPkgs fira-code-pkgs [ self.overlay nur.overlays.default ];
      utsushiPkgs = mkPkgs utsushi-nixpkgs [ ];
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
          firaCodePkgs = firaCodePkgs;
          custom = myCustomPkgs;
          unstable = uPkgs;
          utsuhiPkgs = utsushiPkgs;
          user = self.packages."${system}";
        };

      overlays =
        mapModules ./overlays import;

      packages."${system}" =
        mapModules ./packages (p: pkgs.callPackage p { })
        // mapModules ./new/packages (p: pkgs.callPackage p { });

      nixosModules =
        {
          dotfiles = import ./.;
        }
        // mapModulesRec ./modules import
        // mapModulesRec ./new/modules import;

      nixosConfigurations =
        mapHosts ./hosts { };
    };
}
