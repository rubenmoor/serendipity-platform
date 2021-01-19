{ mkDerivation, base, bytestring, common, heterocephalus
, monad-logger, mtl, network, optparse-applicative, persistent
, persistent-mysql, persistent-template, resource-pool, resourcet
, servant, servant-server, stdenv, text, text-show, time, warp
}:
mkDerivation {
  pname = "backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring common heterocephalus monad-logger mtl network
    optparse-applicative persistent persistent-mysql
    persistent-template resource-pool resourcet servant servant-server
    text text-show time warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
