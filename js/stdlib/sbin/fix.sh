#!/bin/sh
set -e

MJS_DIR=dist/mjs

mkdir -p "$MJS_DIR"

# Rewrite build/*.js to build/*.mjs
for JS in dist/esm/*.js ; do
  cp "${JS}" "$MJS_DIR/$(basename "${JS}" .js).mjs"
done
cp ./*.mjs "$MJS_DIR/"

# Remove the need for this and instead turn off ESLint or use typescript-eslint
npm run beautify
# npm run format

cd "$MJS_DIR" || exit 1

# Rewrite `from './blah'` to `from './blah.mjs'`
# Usage: sbin/fix.sh
for FILE in *.mjs ; do
  for IMJS in *.mjs ; do
    I="${IMJS%.*}" # strip the .mjs suffix
    sed -i.bak "s#from './$I'#from './$I.mjs'#" "$FILE"
    rm "$FILE.bak"
  done
  # also snag the typedefs
  F="${FILE%.*}" # strip the .mjs suffix
  TYPEDEFS="../esm/$F.d.ts"
  if [ -f "$TYPEDEFS" ] ; then
    cp "$TYPEDEFS" . ;
  else
    echo "no typedefs at $TYPEDEFS"
  fi
done
