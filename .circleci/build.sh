#!/bin/bash
KIDS=0

WORKSPACE="/tmp/workspace/docker"
DONE="/tmp/done"
ARTS="/tmp/artifacts"
mkdir -p "${WORKSPACE}" "${DONE}" "${ARTS}"

waitFor () {
  IMAGE="$1"
  DEP="$2"
  while [ ! -f "${DONE}/${DEP}" ] ; do
    echo "${IMAGE} waits for ${DEP}"
    sleep 10
  done
  echo "${IMAGE} sees ${DEP} is done"
}

IMAGES=()
imagek () {
  IMAGE="$1"
  IMAGES+=("$IMAGE")
  shift 1
  DEPS=( "$@" )
  echo "Scheduling ${IMAGE}"

  for DEP in "${DEPS[@]}" ; do
    waitFor "${IMAGE}" "${DEP}"
  done

  echo "Building ${IMAGE}"
  ./image.sh "${IMAGE}" >>"${ARTS}/${IMAGE}" 2>&1
  echo "Done with ${IMAGE}"
  touch "${DONE}/${IMAGE}"
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

for IMAGE in "${IMAGES[@]}" ; do
  echo "Saving ${IMAGE}"
  docker save "reachsh/${IMAGE}:latest" | gzip > "${WORKSPACE}/${IMAGE}".tar.gz
  echo "Caching ${IMAGE}"
  ../scripts/cache-image.sh "${IMAGE}"
done
