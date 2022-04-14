#!/bin/sh -e
# this script replicates the behavior of testCompileOut in /hs/test/Reach/Test_Compiler.hs
case "$1" in
  *.rsh) RSH="$1" ;;
  *) if [ -f "index.rsh" ]; then 
       RSH="index.rsh"
     else
       echo "pass a .rsh file as the first arg"
       exit 1
     fi
     ;;
esac

REPO="$(realpath "$(dirname "$0")"/..)"
DIRNAME="$(dirname "$(realpath "$RSH")")"
BASENAME="$(basename "$RSH" ".rsh")"
OUTPUT_F="$DIRNAME"/"$BASENAME".txt
STDERR_F="$(mktemp)"
trap 'rm $STDERR_F' EXIT

cd "$DIRNAME"
if [ "$REACH_DOCKER" = "0" ]; then
  # don't use reach script directly here because we do not want to include
  # make's output in the golden file
  make --silent --directory "$REPO"/hs/ hs-build 
  stack --stack-yaml "$REPO"/hs/stack.yaml exec -- \
    reachc --disable-reporting "$RSH" >"$OUTPUT_F" 2>"$STDERR_F" || true
else
  "$REPO"/reach compile --disable-reporting "$RSH" >"$OUTPUT_F" 2>"$STDERR_F" || true
fi

# stderr comes after stdout
# callstack is removed
sed --quiet "/CallStack (from HasCallStack):/q;p " "$STDERR_F" >> "$OUTPUT_F"

# full directory paths are replaced with "."
ESCAPED="$(echo "$DIRNAME" | sed 's/\./\\\./g')"
sed --in-place "s#$ESCAPED#.#g" "$OUTPUT_F"

cat "$OUTPUT_F"
