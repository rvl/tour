{ mkDerivation, aeson, async, base, bytestring, containers
, dependent-map, dependent-sum, dependent-sum-template, http-types
, lens, lucid, miso, mtl, naqsha, network-uri, scientific, servant
, servant-lucid, servant-server, stdenv, text, time, vector, wai
, wai-extra, warp
}:
mkDerivation {
  pname = "tour";
  version = "0.1.0.0";
  src = null;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bytestring containers dependent-map dependent-sum
    dependent-sum-template http-types lens lucid miso mtl naqsha
    network-uri scientific servant servant-lucid servant-server text
    time vector wai wai-extra warp
  ];
  description = "Tour frontend";
  license = stdenv.lib.licenses.gpl3;
}
