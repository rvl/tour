{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjsHEAD", doBenchmark ? false }:
# compiler could also be ghcjsHEAD for 802

let
  inherit (nixpkgs) pkgs;

  resultOld = import (pkgs.fetchFromGitHub {
    owner = "haskell-miso";
    repo = "miso";
    rev = "1593151dd10f26c14dd2cf7cea3489f27a6d8f58";
    sha256 = "1ha0nh9jmy6czlikdb0fay03mir2gyx2dj07lzwfvap7hx7gjb6n";
  }) {};
  result = import /home/rodney/src/haskell-miso/miso { tests = false; };
  tour-ghcjs = pkgs.haskell.packages.ghcjws.callPackage tour {
    miso = result.miso-ghcjs;
  };
  
  tour = ./tour.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage tour {});

in

  if pkgs.lib.inNixShell then drv.env else drv
