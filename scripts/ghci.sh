#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghc \
  --interactive \
  -haddock \
  -v0 \
  -isrc/ \
  -itests/ \
  -fhide-source-paths \
  -XImportQualifiedPost \
  -XLambdaCase \
  -XOverloadedStrings \
  -Wall \
  -ghci-script scripts/ghci.conf
