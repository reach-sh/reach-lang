#!/bin/sh
set -e

MODE="$1"
EXAMPLES=$(find . -mindepth 1 -maxdepth 1 -type d | sort)
HERE=$(dirname "$0")

for e in $EXAMPLES; do
  if ! [ "$MODE" = "run" ]; then
    "${HERE}/one.sh" "$MODE" "$e"
  else
    CONN="$(echo "${REACH_CONNECTOR_MODE:-ETH}" | sed 's/-.*$//')"
    FAIL="$CONN.$(echo "$e" | sed 's/\.\///')"

    printf '\n================================================================================\n'

    if ! "${HERE}/one.sh" run "$e"; then
      if grep -q "$FAIL" "$HERE/../.circleci/xfail.txt"; then
        printf '\n\n *** Ignoring allowed failure for %s ***\n\n' "$FAIL"
      else
        exit 1
      fi
    fi
  fi
done
