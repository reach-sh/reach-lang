#!/bin/sh
# usage:
# ./inspect.sh latest-m1

HERE=.
VER="$1"

# shellcheck disable=SC2013
for IMG in $(cat "$HERE/imgs.txt") ; do
  echo "$IMG:$VER"
  M="$(docker manifest inspect --verbose "reachsh/$IMG:$VER")"
  echo "$M" | jq '.[].Descriptor.platform.architecture' 2>/dev/null \
    || echo "$M" | jq '.Descriptor.platform.architecture' 2>/dev/null
done
