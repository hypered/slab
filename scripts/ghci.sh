#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghc \
  --interactive \
  -XImportQualifiedPost \
  -Wall \
  bin/pughs.hs
