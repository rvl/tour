{ mkDerivation, base, miso, servant, naqsha, lens, dependent-map, dependent-sum-template, async, stdenv }:
mkDerivation {
  pname = "tour";
  version = "0.1.0.0";
  src = null;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base miso servant naqsha lens dependent-map dependent-sum-template async ];
  description = "Tour frontend";
  license = stdenv.lib.licenses.gpl3;
}
