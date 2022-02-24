#!/bin/bash -xe
IMAGE=$1
FILE=$2
shift 2
ARGS=( "$@" )

HERE=$(dirname "$0")
ROOT="${HERE}"/..
mkdir -p .docker-root
ARGS+=( "--build-arg" "REACH_GIT_HASH=$("${HERE}"/git-hash.sh)" )
# shellcheck source=/dev/null
. "${ROOT}"/VERSION
cp -f "${ROOT}"/VERSION .docker-root/
# shellcheck source=/dev/null
. "${ROOT}"/DEPS
cp -f "${ROOT}"/DEPS .docker-root/
ARGS+=( "--build-arg" "SOLC_VERSION=${SOLC_VERSION}" )
ARGS+=( "--build-arg" "SOLC_IMAGE=${SOLC_IMAGE}" )

ARGS+=( "--build-arg" "ALPINE_VERSION=${ALPINE_VERSION}" )
ARGS+=( "--build-arg" "ALPINE_IMAGE=${ALPINE_IMAGE}" )

ARGS+=( "--build-arg" "NODE_VERSION=${NODE_VERSION}" )
ARGS+=( "--build-arg" "NODE_IMAGE=${NODE_IMAGE}" )

ARGS+=( "--build-arg" "DEBIAN_IMAGE=${DEBIAN_IMAGE}" )
ARGS+=( "--build-arg" "DEBIAN_NODE_IMAGE=${DEBIAN_NODE_IMAGE}" )

ARGS+=( "--build-arg" "UBUNTU_IMAGE=${UBUNTU_IMAGE}" )
ARGS+=( "--build-arg" "UBUNTU_NODE_IMAGE=${UBUNTU_NODE_IMAGE}" )

ARGS+=( "--build-arg" "CYPRESS_VERSION=${CYPRESS_VERSION}" )
ARGS+=( "--build-arg" "CYPRESS_IMAGE=${CYPRESS_IMAGE}" )

ARGS+=( "--build-arg" "GOLANG_IMAGE=${GOLANG_IMAGE}" )

ARGS+=( "--build-arg" "REACH_VERSION=${VERSION}" )

ARGS+=( "--build-arg" "Z3_VERSION=${Z3_VERSION}" )

LAYERS=$(grep -E 'FROM .* (as|AS)' "${FILE}" | grep -v ignore | awk -F ' (as|AS) ' '{print $2}')

if [ "${CIRCLE_BRANCH}" = "" ] ; then
  CIRCLE_BRANCH=master
fi

CACHE_FROM=()
dp () {
  if [ "${REACH_BUILD_NO_CACHE}" = "" ] ; then
    docker pull "$1" || true
    CACHE_FROM+=("--cache-from=${1}")
  fi
}
IMAGEC="${IMAGE}:circleci"
dpb () {
  dp "${IMAGEC}-${1}-${CIRCLE_BRANCH}"
  if [ "${CIRCLE_BRANCH}" != "master" ] ; then
    dp "${IMAGEC}-${1}-master"
  fi
}

dpb ""
for i in $LAYERS; do
  dpb "$i"
done

DOCKER_BUILDKIT=1
export DOCKER_BUILDKIT

build_image () {
    LAYER=$1
    TARGET=()
    TAG="--tag=${IMAGEC}-${LAYER}-${CIRCLE_BRANCH}"
    if [ "x${LAYER}" != "x" ]; then
      TARGET+=("--target=${LAYER}")
    fi

    # linux/arm64
    # docker build --platform=linux/amd64 "$TAG" "${TARGET[@]}" "${CACHE_FROM[@]}" "${ARGS[@]}" --file "$FILE" --build-arg BUILDKIT_INLINE_CACHE=1 .
    docker build "$TAG" "${TARGET[@]}" "${CACHE_FROM[@]}" "${ARGS[@]}" --file "$FILE" --build-arg BUILDKIT_INLINE_CACHE=1 .
    # ^ that inline cache might be a bad idea for the "final" one
}

for i in $LAYERS; do
    build_image "$i"
done

build_image ""
docker tag "${IMAGEC}--${CIRCLE_BRANCH}" "${IMAGE}:latest"
docker tag "${IMAGEC}--${CIRCLE_BRANCH}" "${IMAGE}:circleci-${CIRCLE_SHA1}"
