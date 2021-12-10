#!/bin/sh
# usage
# ./push.sh latest-m1

HERE=.
TAG="$1"

for IMG in $(cat "$HERE/imgs.txt") ; do
  docker push "reachsh/$IMG:$TAG"
done
