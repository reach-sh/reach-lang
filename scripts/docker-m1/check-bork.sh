#!/bin/sh
# Usage: ./check-bork.sh
set -ex

HERE=.
# shellcheck disable=SC2013
for IMG in $(cat "$HERE/imgs.txt") ; do
  curl "https://hub.docker.com/v2/repositories/reachsh/$IMG/tags?page_size=1"
done
echo 'looks ok'
