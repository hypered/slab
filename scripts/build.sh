#! /usr/bin/env bash

# Build the binary with different versions of nixpkgs and GHC.
# The commented out versions fail to build.

builds=(
  "23.05 ghc928"
  "23.05 ghc947"
  # "23.05 ghc962"
  # "23.11 ghc928"
  "23.11 ghc948"
  "23.11 ghc962"
  "24.05 ghc928"
  "24.05 ghc948"
  "24.05 ghc965"
  # "24.05 ghc982"
  # "24.05 ghc9101"
  "24.11 ghc928"
  "24.11 ghc948"
  "24.11 ghc966"
  "24.11 ghc983"
  # "24.11 ghc9101"
  "25.05 ghc928"
  "25.05 ghc948" # Later versions than this fail for the statically-linked binary.
  "25.05 ghc967"
  "25.05 ghc984"
  # "25.05 ghc9102"
  # "25.05 ghc9122"
)

for build in "${builds[@]}"; do
  read -r channel compiler <<< "$build"
  echo "$channel $compiler"
  nix-build \
    --attr binaries \
    --argstr channel "$channel" \
    --argstr compiler "$compiler" \
    --no-out-link
done
