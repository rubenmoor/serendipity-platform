{ mkDerivation, aeson, base, bytestring, http-media, servant
, stdenv, text
}:
mkDerivation {
  pname = "common";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring http-media servant text
  ];
  license = stdenv.lib.licenses.bsd3;
}
