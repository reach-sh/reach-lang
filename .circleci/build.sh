#!/bin/bash
KIDS=0

WORKSPACE="/tmp/workspace/docker"
DONE="/tmp/done"
ARTS="/tmp/artifacts"
mkdir -p "${WORKSPACE}" "${DONE}" "${ARTS}"

imagek () {
  IMAGE="$1"
  shift 1
  DEPS=( "$@" )
  echo "Scheduling ${IMAGE}"

  for DEP in "${DEPS[@]}" ; do
    while [ ! -f "${DONE}/${DEP}" ] ; do
      sleep 1
    done
    echo "${IMAGE} sees ${DEP} is done"
  done

  echo "Building ${IMAGE}"
  ./image.sh "${IMAGE}" >>"${ARTS}/${IMAGE}" 2>&1
  touch "${DONE}/${IMAGE}"

  docker save "reachsh/${IMAGE}:latest" | gzip > "${WORKSPACE}/${IMAGE}".tar.gz
  ../scripts/cache-image.sh "${IMAGE}"
}

image () {
  KIDS=$((KIDS + 1))
  imagek "$@" &
}

image "devnet-eth"
image "devnet-cfx"
image "devnet-algo"
image "haskell-build-artifacts" "devnet-algo"
image "reach" "haskell-build-artifacts"
image "reach-cli" "haskell-build-artifacts"
image "js-deps"
image "stdlib" "reach" "js-deps"
image "runner" "stdlib"
image "react-runner" "stdlib" "js-deps"
image "rpc-server" "runner"

while true; do
  wait -n || true
  KIDS=$((KIDS - 1))
  if [ $KIDS -eq 0 ]; then break; fi
done

wait
