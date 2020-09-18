#!/bin/sh
set -e

for FILE in *.mjs ; do
  for IMJS in *.mjs ; do
    I="${IMJS%.*}"
    sed -i.bak "s#from './$I'#from './$I.mjs'#" "$FILE"
    rm "$FILE.bak"
  done
done
