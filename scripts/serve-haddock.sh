#! /usr/bin/env nix-shell
#! nix-shell -i bash -p busybox

HADDOCK_DIR=$(nix-build -A haddock ./default.nix --no-out-link)

echo "Visit http://127.0.0.1:9000/"
httpd -f -p 9000 -h "${HADDOCK_DIR}/share/doc/slab-0.0.2.0/html/"
