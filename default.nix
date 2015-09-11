{ mkDerivation, base, HS2AST, QuickCheck, quickspec, stdenv, tasty
, tasty-quickcheck
}:
mkDerivation {
  pname = "ML4HSFE";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base HS2AST QuickCheck quickspec tasty tasty-quickcheck
  ];
  homepage = "http://chriswarbo.net/git/ml4hsfe";
  description = "ML4PG-like feature extraction for Haskell ASTs";
  license = stdenv.lib.licenses.publicDomain;
}
