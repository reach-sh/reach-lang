#!/bin/sh
# usage:
#   ./pull.sh manifest

HERE=.
TAG="$1"

# shellcheck disable=SC2013
for IMG in $(cat "$HERE/imgs.txt") ; do
  docker rmi "reachsh/$IMG:$TAG"
  docker pull "reachsh/$IMG:$TAG"
done
