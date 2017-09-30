{ nixpkgs ? import <nixpkgs> {} }:

let
  build = import ./build.nix { inherit nixpkgs; };
  frontend = import ./frontend.nix { inherit nixpkgs; };
in
  build
