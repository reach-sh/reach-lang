#!/bin/sh -e
# this script replicates the behavior of testCompileOut in /hs/test/Reach/Test_Compiler.hs
case "$1" in
  *.rsh) ;;
  *) echo "pass a .rsh file as the first arg"; exit 1
esac

# build reachc if not already
REPO="$(realpath "$(dirname "$0")"/..)"
make -s -C "$REPO"/hs/ hs-build

DIRNAME="$(dirname "$(realpath "$1")")"
BASENAME="$(basename "$1" ".rsh")"
OUTPUT_F="$DIRNAME"/"$BASENAME".txt
STDERR_F="$(mktemp)"
trap 'rm $STDERR_F' EXIT

cd "$DIRNAME"
stack --stack-yaml "$REPO"/hs/stack.yaml exec -- reachc --disable-reporting "$BASENAME".rsh \
  >"$OUTPUT_F" 2>"$STDERR_F" || true

# stderr comes after stdout
# callstack is removed
sed --quiet "/CallStack (from HasCallStack):/q;p" "$STDERR_F" >> "$OUTPUT_F"

# full directory paths are replaced with "."
ESCAPED="$(echo "$DIRNAME" | sed 's/\./\\\./g')"
sed --in-place "s#$ESCAPED#.#g" "$OUTPUT_F"

cat "$OUTPUT_F"
