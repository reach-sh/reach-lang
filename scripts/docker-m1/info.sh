#!/bin/sh
# usage:
# ./info.sh latest-m1

HERE=.
TAG="$1"

# shellcheck disable=SC2013
for IMG in $(cat "$HERE/imgs.txt") ; do
  H="$(docker run --entrypoint=/bin/sh "reachsh/$IMG:$TAG" -c 'echo $REACH_GIT_HASH')"
  echo "$IMG:$TAG = $H"
done
