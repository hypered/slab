let

  sources = import ./nix/sources.nix;
  overlays = import ./nix/overlays.nix;

  # Overlays let us override certain packages at a central location.
  nixpkgs = import sources.nixpkgs { };
  nixpkgs-overlayed = import sources.nixpkgs { inherit overlays; };
  hp = nixpkgs-overlayed.haskellPackages;

  contents = import ./nix/contents.nix { inherit nixpkgs; };

  # Niv is great at pinning dependencies in sources.json and computing SHA's etc.
  nix-tooling = with hp; [ (callCabal2nix "niv" sources.niv { }) ];

  # Haskell tools
  haskell-tooling = with hp; [ cabal-install ghcid hlint hasktags fourmolu apply-refact ];

  # Add more as we need them.
  formatters = [ nixpkgs.ormolu nixpkgs.treefmt ] ;

in hp.shellFor {
  packages = p: map (contents.getPkg p) (builtins.attrNames contents.pkgList);
  buildInputs = nix-tooling ++ haskell-tooling ++ formatters;
}
