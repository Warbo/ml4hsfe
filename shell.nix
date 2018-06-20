with import <nixpkgs> {};
runCommand "ml4hs-test-env"
  { buildInputs = [ jq haskell.packages.ghc7103.ghc ]; }
  "exit 1"
