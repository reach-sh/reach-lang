#!/bin/sh
set -e

# Rewrite build/*.js to build/*.mjs
for JS in build/*.js ; do
  mv "${JS}" "$(basename "${JS}" .js).mjs"
done

# Remove the need for this and instead turn off ESLint or use typescript-eslint
npm run beautify
# npm run format # XXX do we need this?

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
  TYPEDEFS="build/$F.d.ts"
  if [ -f "$TYPEDEFS" ] ; then
    cp "$TYPEDEFS" . ;
  else
    echo "no typedefs at $TYPEDEFS"
  fi
done
