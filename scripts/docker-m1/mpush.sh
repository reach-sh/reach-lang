#!/bin/sh
# usage
# ./mpush.sh manifest

HERE=.
TAG="$1"

for IMG in $(cat "$HERE/imgs.txt") ; do
  docker manifest push "reachsh/$IMG:$TAG"
done
