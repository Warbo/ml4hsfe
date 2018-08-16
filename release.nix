with rec {
  sysPkgs = import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; };

  helpers = import sysPkgs.helpersSrc;

  pinned  = import helpers.repo1803 { overlays = [
    (import "${sysPkgs.helpersSrc}/overlay.nix")
    (import ./overlay.nix)
  ]; };
};
{
  inherit (pinned) ML4HSFE;
}
