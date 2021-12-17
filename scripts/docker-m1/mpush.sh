#!/bin/sh
# usage
# ./mpush.sh manifest

HERE=.
TAG="$1"

# shellcheck disable=SC2013
for IMG in $(cat "$HERE/imgs.txt") ; do
  docker manifest push "reachsh/$IMG:$TAG"
done
