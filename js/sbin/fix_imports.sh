#!/bin/sh
set -e

# Rewrite `from './blah'` to `from './blah.mjs'`
# Usage: sbin/fix_imports.sh

for FILE in *.mjs ; do
  for IMJS in *.mjs ; do
    I="${IMJS%.*}" # strip the .mjs suffix
    sed -i.bak "s#from './$I'#from './$I.mjs'#" "$FILE"
    rm "$FILE.bak"
  done
done
