#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghc \
  --interactive \
  -isrc \
  -itests \
  -XImportQualifiedPost \
  -XOverloadedStrings \
  -Wall \
  tests/run-examples.hs
