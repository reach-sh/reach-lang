#!/bin/sh
set -e
set -x

FILE="$1"
shift

for I in "$@" ; do
  sed -i.bak "s#from './$I'#from './$I.mjs'#" "$FILE"
  rm "$FILE.bak"
done
