with builtins;
with import ./overlayed.nix;
import (haskellPackages.haskellSrc2nix {
  name = "ML4HSFE";
  src  = filterSource
    (path: _:
      with rec {
        unwanted = [ ".git" "asv.conf.json" "benchmarks" "dist" "dist-newstyle"
                     "README" "result" ];
        isNix    = lib.hasSuffix ".nix" path;
      };
      !(elem (baseNameOf path) unwanted || isNix))
    ./.;
})
