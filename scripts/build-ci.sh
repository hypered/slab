#! /usr/bin/env nix-shell
#! nix-shell -i bash ../default.nix -A shell

# This is used in .github/workflows/deployment.yml.

set -e

slab build content/
cp -r static/ _site/
cp static/favicon.ico _site/
