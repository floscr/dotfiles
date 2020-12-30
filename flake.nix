{
  inputs =
    {
      nixpkgs.url          = "nixpkgs/master";
      nixpkgs-unstable.url = "nixpkgs/master";

      home-manager.url   = "github:rycee/home-manager/master";
      home-manager.inputs.nixpkgs.follows = "nixpkgs";

      emacs-overlay.url  = "github:nix-community/emacs-overlay";
      nixos-hardware.url = "github:nixos/nixos-hardware";
    };

  outputs = { self, nixpkgs, nixpkgs-unstable, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs;
      uPkgs = nixpkgs-unstable;
    in {
      nixosConfigurations = {
        laptop = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./.
            ./hosts/macbook-air/configuration.nix
          ];
        };
      };
    };
}
