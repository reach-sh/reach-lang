#!/bin/sh
WHICH="$1"
ROOT=".."
TARGET="build-${WHICH}"

case "$WHICH" in
  "haskell-build-artifacts"|"reach"|"reach-cli")
    DIR="hs"
    ;;
  "js-deps"|"stdlib"|"runner"|"react-runner"|"rpc-server")
    DIR="js"
    ;;
  devnet-*)
    DIR="scripts/${WHICH}"
    ;;
  *)
    echo "No image: ${WHICH}"
    exit 1
    ;;
esac
cd "${ROOT}/${DIR}" || exit 1
exec make "${TARGET}"
