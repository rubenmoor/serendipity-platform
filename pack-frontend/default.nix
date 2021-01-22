{ pkgs ? import <nixpkgs> {} }:
let
  serendipity = import ./../default.nix {};
  frontend = serendipity.ghcjs.frontend;
in
  pkgs.stdenv.mkDerivation {
    inherit (frontend) name version;
    src = ./.;
    buildInputs = [ pkgs.closurecompiler ];
    configurePhase = ''
      mkdir -p $out/bin
    '';
    buildPhase = ''
      ${pkgs.closurecompiler}/bin/closure-compiler \
        ${frontend}/bin/frontend.jsexe/all.js \
        --compilation_level=ADVANCED_OPTIMIZATIONS \
        --jscomp_off=checkVars \
        --externs=${frontend}/bin/frontend.jsexe/all.js.externs \
        > $out/bin/all.min.js
    '';
    installPhase = ''
      cp $src/index.html $out/bin
    '';
  }
