{ nixpkgs ? import <nixos-unstable> {} }:

let
  inherit (nixpkgs) pkgs;
  haskellPackages = pkgs.haskell.packages.ghc8102;
  common = haskellPackages.callPackage ../common {};
  gerippe = haskellPackages.callPackage ../../../gerippe {};
  backend = haskellPackages.callPackage ./. { inherit common; inherit gerippe; };
in
  backend.env
