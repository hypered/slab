{
  compiler,
  sources
}:

final: super:
let

  lib = super.lib;
  contents = import ./contents.nix { inherit sources; };
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

  staticOverrides = self: superh: rec {
    mkDerivation = args: superh.mkDerivation (
      if args.pname == "slab"
      then args // {
        doCheck = false;
        doHaddock = false;
        enableLibraryProfiling = false;
        enableExecutableProfiling = false;

        configureFlags = (args.configureFlags or [ ]) ++ [
          "--ghc-option=-optl=-static"
          "--extra-lib-dirs=${final.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${final.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
          "--extra-lib-dirs=${final.zlib.static}/lib"
        ];
        enableSharedExecutables = false;
        enableSharedLibraries = false;
        postInstall = (args.postInstall or "") + ''
          for i in $out/bin/*
          do
            if ldd "$i"
            then
              echo "ldd exited with success for $i. It is probably not statically linked."
              exit 1
            fi
          done
        '';
      }
      else args);
  };

in {
  haskellPackages = super.haskell.packages."${compiler}".override (old: {
    overrides =
      lib.composeExtensions (old.overrides or (_: _: { })) ourOverrides;
  });

  haskellPackagesNoProfiling = super.haskell.packages."${compiler}".override (old: {
    overrides =
      lib.composeExtensions
        (lib.composeExtensions (old.overrides or (_: _: { })) ourOverrides)
        theseOverrides;
  });

  haskellPackagesStatic = super.haskell.packages."${compiler}".override (old: {
    overrides =
      lib.composeExtensions
        (lib.composeExtensions (old.overrides or (_: _: { })) ourOverrides)
        staticOverrides;
  });
}
