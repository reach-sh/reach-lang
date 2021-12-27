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
    TARGET="build"
    ;;
  *)
    echo "No image: ${WHICH}"
    exit 1
    ;;
esac

mkdir -p "/tmp/workspace/docker/${WHICH}"
cd "${ROOT}/${DIR}" || exit 1
make "${TARGET}"
echo "docker save reachsh/${WHICH}:latest..."
docker save "reachsh/${WHICH}:latest" | gzip > "/tmp/workspace/docker/${WHICH}/${WHICH}".tar.gz
