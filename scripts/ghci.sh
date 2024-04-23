#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghc \
  --interactive \
  -isrc \
  -XImportQualifiedPost \
  -XOverloadedStrings \
  -Wall \
  bin/pughs.hs
