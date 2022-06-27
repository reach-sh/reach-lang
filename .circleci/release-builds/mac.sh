#!/usr/bin/env bash
set -e
cd hs || exit

# Install command dependencies
make install-mo
curl -sSL https://get.haskellstack.org/ | sh
HOMEBREW_NO_AUTO_UPDATE=1 brew install wget

# Compile
make hs-release

# Gather artifacts
REACHPC="$(stack exec -- which reachpc)"
REACHC="$(stack exec -- which reachc)"
strip "$REACHPC" "$REACHC"
mkdir -p /tmp/artifacts
mv "$REACHPC" "$REACHC" /tmp/artifacts
for art in /tmp/artifacts/*; do gzip "$art"; done
