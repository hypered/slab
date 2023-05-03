# Central overlay that supplies all overlays that:
# 1. Make this package available.
# 2. Provide this particular package with a fixed point of overlayed packages,
#    if they become needed.

let

  sources = import ./sources.nix;
  #inherit (sources) commence hypered-design;

  getOverlays = pkg : import "${pkg}/nix/overlays.nix";

  # We can overlay Haskell packages here.
  haskellOverlays = []
    #++ getOverlays commence
    #++ getOverlays hypered-design
    ;

in haskellOverlays ++ [ (import ./overlay.nix) ]
