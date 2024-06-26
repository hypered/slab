self: super:
let

  lib = super.lib;
  sources = import ./sources.nix;
  contents = import ./contents.nix { nixpkgs = super; };
  inherit (super.lib.attrsets) mapAttrs;

  ourOverrides = selfh: superh:
    let
      callCabalOn = name: dir:
        selfh.callCabal2nix "${name}" dir { };

    in mapAttrs callCabalOn contents.pkgList;

  # Don't build profiling version for Slab. This reduces the build time
  # from 2m40s to 1m40s.
  theseOverrides = self: superh: rec {
    mkDerivation = args: superh.mkDerivation (
      if args.pname == "slab"
      then args // {
        doCheck = false;
        doHaddock = false;
        enableLibraryProfiling = false;
        enableExecutableProfiling = false;
      }
      else args);
  };

in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides =
      lib.composeExtensions (old.overrides or (_: _: { })) ourOverrides;
  });

  haskellPackagesNoProfiling = super.haskellPackages.override (old: {
    overrides =
      lib.composeExtensions
        (lib.composeExtensions (old.overrides or (_: _: { })) ourOverrides)
        theseOverrides;
  });
}
