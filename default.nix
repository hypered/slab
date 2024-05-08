let
  sources = import ./nix/sources.nix;
  overlays = import ./nix/overlays.nix;
  nixpkgs = import sources.nixpkgs { inherit overlays; };
in rec
  {
    # Build with nix-build -A <attr>
    binaries = nixpkgs.haskellPackages.language-pug;

    # A shell to try out our binaries
    # Run with nix-shell default.nix -A shell
    shell = nixpkgs.mkShell {
      buildInputs = [
        binaries
        nixpkgs.busybox # for httpd
      ];
      shellHook = ''
        source <(pughs --bash-completion-script `which pughs`)
      '';
    };
  }
