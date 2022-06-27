#!/usr/bin/env bash
set -e

# Install command dependencies
choco install make wget
curl -sSL https://get.haskellstack.org/ | bash
. DEPS && wget -O /bin/mo "$MO_URL"

# Compile
cd hs || exit
make hs-release

# Gather artifacts
REACHPC="$(stack exec -- which reachpc)"
REACHC="$(stack exec -- which reachc)"
mkdir -p /c/tmp/artifacts
mv "$REACHC" "$REACHPC" /c/tmp/artifacts
for art in /c/tmp/artifacts/*; do 7z -sdel a "$art".gz "$art"; done
