#!/bin/sh
# usage: (src dst)
# ./retag.sh latest latest-m1
set -ex

HERE=.
FROM="$1"
TO="$2"

# shellcheck disable=SC2013
for IMG in $(cat "$HERE/imgs.txt") ; do
  docker tag "reachsh/$IMG:$FROM" "reachsh/$IMG:$TO"
done
