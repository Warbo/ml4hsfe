with import ./overlayed.nix;
stdenv.mkDerivation {
  name        = "ML4HSFE-shell";
  buildInputs = [ asv-nix git hsPkgs.ghc ];
}
