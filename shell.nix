{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, HS2AST, QuickCheck, quickspec, stdenv
      , tasty, tasty-quickcheck
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
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
