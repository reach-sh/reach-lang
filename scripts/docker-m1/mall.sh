#!/bin/sh
# (mall = manifest all)
# usage:
# ./mall.sh 2022-02-02 ABC1234

HERE=.
DATEVER="$1"
HASHVER="$2"

for VER in $(cat "$HERE/versions.txt") "$DATEVER" "$HASHVER" ; do
  ./manifest.sh "$VER" latest-amd64 latest-arm64
  ./mpush.sh "$VER"
done
