{ nixpkgs ? import <nixos-unstable> {} }:

let
  inherit (nixpkgs) pkgs;
  haskellPackages = pkgs.haskell.packages.ghc8102;
  common = haskellPackages.callPackage ../common {};
  backend = haskellPackages.callPackage ./. { inherit common; };
in
  backend.env
