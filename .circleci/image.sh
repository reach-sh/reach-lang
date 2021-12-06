!/bin/sh
WHICH="$1"
ROOT=".."
TARGET="build-${WHICH}"

case "$WHICH" in
  haskell-build-artifacts*|"reach"|"reach-cli")
    DIR="hs"
    ;;
  "js-deps"|"runner"|"react-runner"|"rpc-server"|stdlib*)
    DIR="js"
    ;;
  devnet-algo*)
    DIR="scripts/devnet-algo"
    ;;
  devnet-cfx*)
    DIR="scripts/devnet-cfx"
    ;;
  devnet-eth*)
    DIR="scripts/devnet-eth"
    ;;
  *)
    echo "No image: ${WHICH}"
    exit 1
    ;;
esac
cd "${ROOT}/${DIR}" || exit 1
exec make "${TARGET}"