{ inputs, config, lib, pkgs, ... }:

with lib;
with lib.my;
with inputs;
{
  imports =
    [
      home-manager.nixosModules.home-manager
      "${inputs.secrets}/private.nix"
    ]
    ++ (mapModulesRec' (toString ./modules) import);

  # Common config for all nixos machines; and to ensure the flake operates soundly
  environment.variables.DOTFILES = dotFilesDir;

  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
    nixPath = [
      "nixpkgs=${nixpkgs}"
      "nixpkgs-unstable=${nixpkgs-unstable}"
      "nixpkgs-virtualbox=${nixpkgs-virtualbox}"
      "nixpkgs-overlays=${dotFilesDir}/overlays"
      "home-manager=${home-manager}"
      "dotfiles=${dotFilesDir}"
    ];
    binaryCaches = [
      "https://nix-community.cachix.org"
    ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    registry = {
      nixos.flake = nixpkgs;
      nixpkgs.flake = nixpkgs-unstable;
      virtualboxPkgs.flake = nixpkgs-virtualbox;
    };
    useSandbox = true;
  };
  system.configurationRevision = mkIf (self ? rev) self.rev;
  system.stateVersion = "20.09";

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
