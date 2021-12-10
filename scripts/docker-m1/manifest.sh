#!/bin/sh

M="$1"
AMD="$2"
ARM="$3"
HERE=.

for IMG in $(cat "$HERE/imgs.txt") ; do
  docker manifest rm "reachsh/$IMG:$M"
  docker manifest create "reachsh/$IMG:$M" \
    "reachsh/$IMG:$AMD" \
    "reachsh/$IMG:$ARM"
done
