#!/bin/sh
set -e

# A tool for visual inspection of diffs between index.rsh
# in examples/ vs react-examples/
# Perhaps we'll make them identical in the future,
# but for now there are small differences.

EXAMPLES=$(find . -mindepth 1 -maxdepth 1 -type d | sort)

for e in $EXAMPLES ; do
  FILE="$e/index.rsh"
  echo "======== $FILE ========"
  diff "../examples/$FILE" "$FILE" || :
done

