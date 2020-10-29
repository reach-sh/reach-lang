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
  OTHER="../examples/$FILE"
  if [ "$e" = "./tut-8" ] ; then
    OTHER="../examples/tut-7/index.rsh"
  fi
  diff "$OTHER" "$FILE" || :
done

