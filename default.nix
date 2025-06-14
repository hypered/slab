{
  channel ? "23.05",
  compiler ? "ghc928",
  sources ? import ./nix/${channel}/nix/sources.nix
}:
let
  overlays = import ./nix/overlays.nix { inherit compiler sources; };
  nixpkgs = import sources.nixpkgs { inherit overlays; };
in rec
  {
    # Build with nix-build -A <attr>
    binaries = nixpkgs.haskellPackages.slab;
    haddock = nixpkgs.haskellPackages.slab.doc;

    static-binaries = nixpkgs.pkgsMusl.haskellPackagesStatic.slab;

    # A shell to try out our binaries
    # Run with nix-shell default.nix -A shell
    shell = nixpkgs.mkShell {
      buildInputs = [
        binaries
        nixpkgs.busybox # for httpd
        nixpkgs.dart-sass
      ];
      shellHook = ''
        source <(slab --bash-completion-script `which slab`)
      '';
    };
  }
