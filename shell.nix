{
  channel ? "25.05",
  compiler ? "ghc948",
  sources ? import ./nix/${channel}/nix/sources.nix
}:
let

  overlays = import ./nix/overlays.nix { inherit compiler sources; };

  # Overlays let us override certain packages at a central location.
  nixpkgs = import sources.nixpkgs { };
  nixpkgs-overlayed = import sources.nixpkgs { inherit overlays; };
  hp = nixpkgs-overlayed.haskellPackages;

  contents = import ./nix/contents.nix { inherit sources; };

  # Niv is great at pinning dependencies in sources.json and computing SHA's etc.
  nix-tooling = with hp; [ (callCabal2nix "niv" sources.niv { }) ];

  # Haskell tools
  haskell-tooling = with nixpkgs.haskellPackages; [
    cabal-install ghcid hlint hasktags fourmolu apply-refact
  ];

  # Add more as we need them.
  formatters = [ nixpkgs.treefmt ] ;

in hp.shellFor {
  packages = p: map (contents.getPkg p) (builtins.attrNames contents.pkgList);
  buildInputs = nix-tooling ++ haskell-tooling ++ formatters;
}
