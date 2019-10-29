# default.nix
{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  packages = {
    frontend = ./frontend;
    clay = ./clay;
  };

  shells = {
    ghc = ["frontend"];
    ghcjs = ["frontend"];
  };
})