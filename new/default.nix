{ inputs, config, lib, pkgs, ... }:

with lib;
with lib.my;
with inputs;
{
  imports =
    [
      home-manager.nixosModules.home-manager
    ]
    ++ (mapModulesRec' (toString ./modules) import);

  # Common config for all nixos machines; and to ensure the flake operates soundly
  environment.variables.DOTFILES = dotFilesDir;

  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
  nix =
    let
      filteredInputs = filterAttrs (n: _: n != "self") inputs;
      nixPathInputs = mapAttrsToList (n: v: "${n}=${v}") filteredInputs;
      registryInputs = mapAttrs (_: v: { flake = v; }) filteredInputs;
    in
    {
      package = pkgs.nixFlakes;
      extraOptions = "experimental-features = nix-command flakes";
      nixPath = nixPathInputs ++ [
        "nixpkgs-overlays=${config.dotfiles.dir}/overlays"
        "dotfiles=${config.dotfiles.dir}"
      ];
      settings = {
        substituters = [
          "https://cachix.org/api/v1/cache/emacs"
          "https://cache.nixos.org"
          "https://nix-community.cachix.org"
          "https://nixpkgs.cachix.org"
        ];
        trusted-public-keys = [
          "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "nixpkgs.cachix.org-1:q91R6hxbwFvDqTSDKwDAV4T5PxqXGxswD8vhONFMeOE="
        ];
        auto-optimise-store = true;
      };
      registry = registryInputs // { dotfiles.flake = inputs.self; };
    };
  system.configurationRevision = with inputs; mkIf (self ? rev) self.rev;
  system.stateVersion = "21.05";

  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.configurationLimit = 10;
    systemd-boot.enable = mkDefault true;
    timeout = 1;
  };

  environment.systemPackages = with pkgs; [
    cached-nix-shell
    coreutils
    git
    gnumake
    killall
    vim
    wget
    nvd # Nix version diff tool
  ];
}
