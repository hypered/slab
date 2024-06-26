let
  sources = import ./nix/sources.nix;
  overlays = import ./nix/overlays.nix;
  nixpkgs = import sources.nixpkgs { inherit overlays; };
in rec
  {
    # Build with nix-build -A <attr>
    binaries = nixpkgs.haskellPackages.slab;
    haddock = nixpkgs.haskellPackages.slab.doc;

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
