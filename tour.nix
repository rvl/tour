{ mkDerivation, base, miso, servant, naqsha, stdenv }:
mkDerivation {
  pname = "tour";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base miso servant naqsha ];
  description = "Tour frontend";
  license = stdenv.lib.licenses.gpl3;
}
