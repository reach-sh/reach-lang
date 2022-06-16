#!/bin/sh

HERE=$(dirname "$0")
ROOT="${HERE}"/..

for DIR in hs/src js scripts/devnet-eth scripts/devnet-algo scripts/devnet-cfx; do
  printf "%s\t %s\n" "$DIR" "$(git log --pretty=format:'%cr' "$ROOT/$DIR" | head -n 1)"
done
