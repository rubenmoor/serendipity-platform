# default.nix
{ system ? builtins.currentSystem, unstable ? import <nixos-unstable> {} } :
(import ./reflex-platform { inherit system; }).project ({pkgs, ...}: {
  useWarp = true;
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };
  shellToolOverrides = ghc: super: {
    haskell-language-server = unstable.haskell-language-server;
  };
})
