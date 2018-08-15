with rec {
  sysPkgs = import <nixpkgs> {};
  helpers = sysPkgs.fetchgit {
    url    = http://chriswarbo.net/git/nix-helpers.git;
    rev    = "ed8379a";
    sha256 = "1ifyz49x9ck3wkfw3r3yy8s0vcknz937bh00033zy6r1a2alg54g";
  };
  mkPkgs = path: import path {
    overlays = [
      # Load our helper functions
      (import "${helpers}/overlay.nix")

      # Force system's Nix, to ensure compatibility with whatever's running us
      (self: super: { inherit (sysPkgs) nix; })
    ];
  };
};

# Load a pinned version of nixpkgs (except for 'nix') with our helpers available
with mkPkgs (mkPkgs <nixpkgs>).repo1803;
with builtins;
with rec {
  src = filterSource (path: _: !(elem (baseNameOf path) [
                       ".git" "dist" "dist-newstyle"
                     ]))
                     ./.;

  HS2AST = latestGit {
    url = http://chriswarbo.net/git/hs2ast.git;
  };

  hsPkgs = { extra ? (_: _: {}) }: haskell.packages.ghc7103.override (old: {
    overrides = self: super: extra self super // {
      # Hackage dependencies

      # Must be < 2.10 and 2.9 causes conflicts elsewhere
      QuickCheck = self.callHackage "QuickCheck" "2.8.2" {};

      # This must be < 2
      quickspec  = self.callHackage "quickspec"  "0.9.6" {};

      # Non-Hackage dependencies

      HS2AST  = self.callPackage (self.haskellSrc2nix {
                                   name = "HS2AST";
                                   src  = HS2AST;
                                 }) {};

      # Nix overrides

      # Test suite dependencies clash
      aeson = haskell.lib.dontCheck super.aeson;

      # This requires semigroups if GHC < 8, but was generated using GHC 8
      system-filepath = haskell.lib.addBuildDepend super.system-filepath
                                                   self.semigroups;

      # Ourselves

      ML4HSFE = self.callPackage (self.haskellSrc2nix {
                                   inherit src;
                                   name = "ML4HSFE";
                                 }) {};
    };
  });
};
{
  package = (hsPkgs {}).ML4HSFE;
}
