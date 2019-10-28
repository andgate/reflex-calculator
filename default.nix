# default.nix
{ pkgs ? import <nixpkgs> {}, system ? builtins.currentSystem }:
let
  plat = (import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
      packages = {
        frontend = ./frontend;
        clay = ./clay;
      };

      shells = {
        ghc = ["frontend"];
        ghcjs = ["frontend"];
      };
    });
in
  pkgs.stdenv.mkDerivation {
    inherit (plat) name version;
    buildCommand = ''
      mkdir -p $out
      cp ${plat}/bin/foo.jsexe/{rts,lib,out,runmain}.js $out
      ${pkgs.closurecompiler}/bin/closure-compiler \
        ${plat}/bin/foo.jsexe/all.js \
        --compilation_level=ADVANCED_OPTIMIZATIONS \
        --jscomp_off=checkVars \
        --externs=${plat}/bin/foo.jsexe/all.js.externs \
        > $out/all.min.js
    '';
  }