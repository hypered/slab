let
  sources = import ./nix/sources.nix;
  overlays = import ./nix/overlays.nix;
  nixpkgs = import sources.nixpkgs { inherit overlays; };
in rec
  {
    # Build with nix-build -A <attr>
    binaries = nixpkgs.haskellPackages.pughs;
  }
