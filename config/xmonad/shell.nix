{ pkgs ? import <nixpkgs> { } }:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (pkgs: [
    pkgs.xmonad
    pkgs.xmonad-extras
    pkgs.xmonad-contrib
    pkgs.taffybar
  ]);
in
pkgs.mkShell {
  buildInputs = [ ghc ];
}
