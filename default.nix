# default.nix
{ system ? builtins.currentSystem, unstable ? import <nixos-unstable> {} } :
(import ./reflex-platform { inherit system; }).project ({pkgs, ...}: {
  useWarp = true;
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
    gerippe = ../../gerippe;
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };
  shellToolOverrides = ghc: super: {
    haskell-language-server = unstable.haskell-language-server;
  };
  overrides = self: super: {
    gerippe = self.callPackage ../../gerippe {};
    servant-reflex = pkgs.fetchFromGitHub {
      owner = "imalsogreg";
      repo = "servant-reflex";
      rev = "master";
      sha256 = "0issnp95rnji3v9qifr0brypxsvmjkzanfq5lscj68lynnjv37g0";
    };
  };
})
