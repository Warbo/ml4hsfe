{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, atto-lisp, attoparsec, base
      , containers, ghc, HS2AST, QuickCheck, quickspec, scientific
      , stdenv, stringable, tasty, tasty-quickcheck, text
      , unordered-containers, vector
      }:
      mkDerivation {
        pname = "ML4HSFE";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson atto-lisp attoparsec base HS2AST QuickCheck scientific
          stringable text unordered-containers vector
        ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [
          aeson atto-lisp base containers ghc HS2AST QuickCheck quickspec
          stringable tasty tasty-quickcheck unordered-containers vector
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
