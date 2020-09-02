#!/bin/sh
set -e

MODE="$1"
EXAMPLES=$(find . -mindepth 1 -maxdepth 1 -type d | sort)
HERE=$(dirname "$0")

for e in $EXAMPLES ; do
  "${HERE}/one.sh" "$MODE" "$e"
done
