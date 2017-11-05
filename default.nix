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
          naqsha
          safe
          aeson
          yaml
          warp
          wai-app-static
          optparse-applicative
        ]);

   bowerComponents = pkgs.buildBowerComponents {
    name = "frontend";
    generated = ./frontend/bower-packages.nix;
    src = builtins.filterSource (name: path: baseNameOf (toString name) == "bower.json") ./frontend;
  };

  buildTools = with pkgs; [
    gpsbabel
    xmlstarlet
    sassc
    closurecompiler
    zopfli
  ];

in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc ] ++ buildTools;
  shellHook = ''
    eval $(egrep ^export ${ghc}/bin/ghc)
    export BOWER_COMPONENTS=${bowerComponents}
  '';

  passthru = {
    frontend = import ./frontend.nix {
      inherit nixpkgs;
    };
  };
}
