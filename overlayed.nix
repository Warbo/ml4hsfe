# Instantiates ./overlay.nix with a pinned, known-good revision of nixpkgs
with rec {
  sysPkgs = import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; };

  helpers = import sysPkgs.helpersSrc;
};
import helpers.repo1803 {
  overlays = [
    (import "${sysPkgs.helpersSrc}/overlay.nix")
    (import ./overlay.nix)
  ];
}
