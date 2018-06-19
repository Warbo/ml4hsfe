with import <nixpkgs> {};
with {
  runWeka = callPackage (fetchFromGitHub {
    owner  = "Warbo";
    repo   = "run-weka";
    rev    = "c4033ce";
    sha256 = "1zdmzznlrdz6ydsd2bm18bjb1xpiq840dvb6x64m4vhxddl0gg33";
  }) {};
};
runCommand "ml4hs-test-env"
  { buildInputs = [ runWeka jq haskell.packages.ghc7103.ghc ]; }
  "exit 1"
