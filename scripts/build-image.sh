#!/bin/bash -xe
IMAGE=$1
FILE=$2
shift 2
ARGS=( "$@" )

if [ "$REACH_BUILD_PLATFORM" != "" ] ; then
  ARGS+=( "--platform" "$REACH_BUILD_PLATFORM" )
fi

MD5=md5sum; if [ ! "$(command -v $MD5)" ]; then MD5=md5; fi

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

REACH_OPEN=${REACH_OPEN:-Y}
if [ "${REACH_OPEN}" = "n" ] ; then
  REACH_HS_STACK_ARGS="--flag reach:everest"
fi
ARGS+=( "--build-arg" "REACH_HS_STACK_ARGS=${REACH_HS_STACK_ARGS}" )

LAYERS=$(grep -E 'FROM .* (as|AS)' "${FILE}" | grep -v ignore | awk -F ' (as|AS) ' '{print $2}')

if [ "${CIRCLE_BRANCH}" = "" ] ; then
  CIRCLE_BRANCH=master
fi

if [ "${CIRCLE_BRANCH}" != "master" ]; then 
  MD5_BRANCH=$(echo "${GITBRANCH}" | ${MD5} | awk '{print $1}')
else
  MD5_BRANCH="${CIRCLE_BRANCH}"
fi

IMAGEC="${IMAGE}:circleci"
if [ "${REGISTRYC}" != "" ] ; then
  IMAGEC=${IMAGEC//reachsh/${REGISTRYC}}
fi

CACHE_FROM=()
dp () {
  docker pull "$1" || true
  CACHE_FROM+=("--cache-from=${1}")
}
dpb () {
  dp "${IMAGEC}-${1}-${MD5_BRANCH}"
  if [ "${CIRCLE_BRANCH}" != "master" ] ; then
    dp "${IMAGEC}-${1}-master"
  fi
}
if [ "${REACH_BUILD_NO_CACHE}" = "" ] ; then
  dpb ""
  for i in $LAYERS; do
    dpb "$i"
  done
fi

DOCKER_BUILDKIT=1
export DOCKER_BUILDKIT

build_image () {
    LAYER=$1
    TARGET=()
    TAG="--tag=${IMAGEC}-${LAYER}-${MD5_BRANCH}"
    if [ "x${LAYER}" != "x" ]; then
      TARGET+=("--target=${LAYER}")
    fi

    # linux/arm64
    # docker build --platform=linux/amd64 "$TAG" "${TARGET[@]}" "${CACHE_FROM[@]}" "${ARGS[@]}" --file "$FILE" --build-arg BUILDKIT_INLINE_CACHE=1 .

    # We are using `tar` so that we can have symlinks in the repo, which are
    # used right now for Everest
    tar -ch . | docker build "$TAG" "${TARGET[@]}" "${CACHE_FROM[@]}" "${ARGS[@]}" --file "$FILE" --build-arg BUILDKIT_INLINE_CACHE=1 --pull=false -
    # ^ that inline cache might be a bad idea for the "final" one
}

for i in $LAYERS; do
    build_image "$i"
done

build_image ""
docker tag "${IMAGEC}--${MD5_BRANCH}" "${IMAGE}:latest"
docker tag "${IMAGEC}--${MD5_BRANCH}" "${IMAGEC}-${CIRCLE_SHA1}"
