{ pkgs ? import <nixpkgs> { config = { allowBroken = true; }; } }:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (pkgs: [
    pkgs.xmonad
    pkgs.haskell-language-server
    pkgs.xmonad-extras
    pkgs.xmonad-contrib

  ]);
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
    # haskellPackages.brittany
    haskellPackages.stylish-haskell
  ];
}
