{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc810" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./proofide.nix {}
