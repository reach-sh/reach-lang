#!/bin/sh
# usage
# ./push.sh latest-m1

HERE=.
TAG="$1"

# shellcheck disable=SC2013
for IMG in $(cat "$HERE/imgs.txt") ; do
  docker push "reachsh/$IMG:$TAG"
done
