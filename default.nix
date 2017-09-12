{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
          shake
          turtle
          hxt
          hxt-xpath
          hxt-xslt
          hxt-relaxng
          hxt-expat
          dhall
          naqsha
          safe
          aeson
          yaml
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc pkgs.gpsbabel pkgs.xmlstarlet ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
