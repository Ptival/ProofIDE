{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjsHEAD" }:
let proofide = (import ./default.nix { inherit nixpkgs compiler; }).env; in
nixpkgs.stdenv.lib.overrideDerivation proofide (old: {
  buildInputs = (with nixpkgs; [
    cabal-install
  ]);
})
