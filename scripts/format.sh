#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

find bin/ src/ tests/ -name '*.hs' -exec fourmolu -q -i {} \;
