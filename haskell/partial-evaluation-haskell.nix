{ mkDerivation, array, base, criterion, ghc-prim, stdenv }:
mkDerivation {
  pname = "partial-evaluation-haskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ array base criterion ghc-prim ];
  license = stdenv.lib.licenses.bsd3;
}
