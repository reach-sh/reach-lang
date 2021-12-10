#!/bin/sh
# usage:
# ./m1-stuff.sh 2022-02-02 abc1234

# DATEVER="$1"
# HASHVER="$2"

# run this after building all images so that 'latest' is the m1 stuff
./retag.sh latest latest-arm64
./push.sh latest-arm64
./reach update
./retag.sh latest latest-amd64
# ./info.sh latest-amd64 # visually inspect that all issue platform warning 
./push.sh latest-amd64
# ./inspect.sh latest-amd64 # visually inspect that all are amd64
# ./inspect.sh latest-arm64 # visually inspect that all are arm64
# TODO use latest instead of manifest
./manifest.sh manifest latest-amd64 latest-arm64
# TODO update all versions w/ the manifest
# for VER in $(cat "$HERE/versions.txt") $DATEVER $HASHVER ; do
#   ./retag.sh manifest "$VER"
# done
# why this instead of regular docker push?
./mpush.sh manifest
# is this needed?
./pull.sh manifest
# XXX not this
./retag.sh manifest latest
# XXX yes this (?) needs to be mpush (?)
# for VER in $(cat "$HERE/versions.txt") $DATEVER $HASHVER ; do
#   ./push.sh "$VER"
# done
