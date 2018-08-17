with import <nixpkgs> {};
stdenv.mkDerivation {
  name        = "ML4HSFE-shell";
  buildInputs = [ haskell.packages.ghc7103.ghc ];
}
