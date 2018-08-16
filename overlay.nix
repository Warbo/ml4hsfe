self: super:

with builtins;
with {
  mkHs = { profile ? false }: self.haskell.packages.ghc7103.override (old: {
    overrides = self.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (helf: huper: {
        # Toggle profiling support
        mkDerivation = if profile
                          then (args: huper.mkDerivation (args // {
                                 enableLibraryProfiling = true;
                               }))
                          else huper.mkDerivation;

        # Hackage dependencies

        # Must be < 2.10 and 2.9 causes conflicts elsewhere
        QuickCheck = helf.callHackage "QuickCheck" "2.8.2" {};

        # This must be < 2
        quickspec  = helf.callHackage "quickspec"  "0.9.6" {};

        # Non-Hackage dependencies

        HS2AST  = helf.callPackage
          (helf.haskellSrc2nix {
            name = "HS2AST";
            src  = self.fetchgit {
              url    = http://chriswarbo.net/git/hs2ast.git;
              rev    = "f48063e";
              sha256 = "1jg62a71mlnm0k2sjbjhf3n5q2c4snlbaj5dlrhdg44kxiyilx9x";
            };
          }) {};

        # Nix overrides

        # Test suite dependencies clash
        aeson = self.haskell.lib.dontCheck huper.aeson;

        # This requires semigroups if GHC < 8, but was generated using GHC 8
        system-filepath = self.haskell.lib.addBuildDepend huper.system-filepath
                                                          helf.semigroups;

        # Ourselves

        ML4HSFE =
          with rec {
            pkg = helf.callPackage
              (helf.haskellSrc2nix {
                name = "ML4HSFE";
                src  = filterSource
                  (path: _:
                    with rec {
                      unwanted = [ ".git" "asv.conf.json" "benchmarks" "dist"
                                   "dist-newstyle" "README" "result" ];
                      isNix    = self.lib.hasSuffix ".nix" path;
                    };
                    !(elem (baseNameOf path) unwanted || isNix))
                  ./.;
              }) {};

            profiled = self.haskell.lib.overrideCabal pkg (drv: {
              configureFlags = [
                "--ghc-option=-rtsopts" "--ghc-option=-auto-all"
              ];
              enableExecutableProfiling = true;
              enableLibraryProfiling    = true;
            });
          };
          if profile then profiled else pkg;
    });
  });
};
{
  helpersSrc = self.fetchgit {
    url    = http://chriswarbo.net/git/nix-helpers.git;
    rev    = "ed8379a";
    sha256 = "1ifyz49x9ck3wkfw3r3yy8s0vcknz937bh00033zy6r1a2alg54g";
  };

  hsPkgs = mkHs {};

  hsProfiled = mkHs { profile = true; };

  memoryProfile = self.runCommand "ml4hsfe-memory-profile"
    {
      buildInputs = [ self.ghostscript self.hsProfiled.ML4HSFE ];
      example     = ./examples/ml4hsfe-outer-loop-example-input-large.json;
      HEIGHT      = "10";
      WIDTH       = "10";
    }
    ''
      mkdir "$out"
      cd "$out"
      ml4hsfe-outer-loop +RTS -hy -i0.01 -RTS < "$example" > output.json
      "${self.hsPkgs.ghc}/bin/hp2ps" < *.hp > heap.ps
      ps2pdf heap.ps
    '';

  inherit (self.hsPkgs) ML4HSFE;

  # Force system's Nix, to ensure compatibility with whatever's running us
  inherit (import <nixpkgs> {}) nix;

  ml4hsfeTests = self.callPackage ./tests.nix {};
}
