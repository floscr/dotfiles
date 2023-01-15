{
  description = "A grossly incandescent nixos config.";

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

      emacs-overlay.url = "github:nix-community/emacs-overlay/80db8e4e9f25e81662a244a96029f3427fe3d5b9";
      nur.url = "github:nix-community/NUR";

      org_print_scan.url = "github:floscr/org_print_scan";
      rofi_org_bookmarks.url = "github:floscr/rofi_org_bookmarks";
      rofi_cmder.url = "github:floscr/rofi_cmder";
    };

  outputs =
    inputs @ { self
    , home-manager
    , nixpkgs
    , nixpkgs-unstable
    , nur
    , org_print_scan
    , emacs-overlay
    , rofi_cmder
    , rofi_org_bookmarks
    , nim-utils
    , ...
    }:
    let
      inherit (lib) attrValues genAttrs;
      inherit (lib.my) mapModules mapModulesRec mapHosts mapConfigurations;

      supportedSystems = rec {
        darwin = [ "x86_64-darwin" "aarch64-darwin" ];
        linux = [ "x86_64-linux" "aarch64-linux" ];
        all = darwin ++ linux;
      };

      mkPkgs = pkgs: extraOverlays: system:
        import pkgs {
          inherit system;
          overlays = extraOverlays ++ (lib.attrValues self.overlays);
        };

      pkgs = genAttrs supportedSystems.all
        (mkPkgs nixpkgs [
          self.overlay
          # nur.overlay
          emacs-overlay.overlay
        ]);
      pkgsUnstable = genAttrs supportedSystems.all
        (mkPkgs nixpkgs-unstable [ ]);

      lib = nixpkgs.lib.extend
        (self: super: { my = import ./lib { inherit pkgs inputs; lib = self; }; });
    in
    {
      lib = lib.my;

      overlay = _:
        { system, ... }: {
          unstable = pkgsUnstable.${system};
          my = self.packages.${system};
        };
      overlays = mapModules ./overlays import;


      packages =
        let
          mkPackages = system:
            mapModules ./packages (p: pkgs.${system}.callPackage p { });
        in
        genAttrs supportedSystems.all mkPackages;

      nixosModules = {
        dotfiles = import ./.;
      } // mapModulesRec ./modules import;

      # NixOS host configurations.
      nixosConfigurations =
        mapConfigurations supportedSystems.linux ./hosts/linux;
    };
}
