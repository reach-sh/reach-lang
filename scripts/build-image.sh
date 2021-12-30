#!/bin/sh
IMAGE=$1
FILE=$2
shift 2
ARGS="$*"
ARGS="${ARGS} --build-arg REACH_GIT_HASH=$(../git-hash.sh)"

. ../VERSION
. ../DEPS
ARGS="${ARGS} --build-arg SOLC_VERSION=${SOLC_VERSION}"
ARGS="${ARGS} --build-arg ALPINE_VERSION=${ALPINE_VERSION}"
ARGS="${ARGS} --build-arg NODE_VERSION=${NODE_VERSION}"
ARGS="${ARGS} --build-arg REACH_VERSION=${VERSION}"

LAYERS=$(grep -E 'FROM .* as' "${FILE}" | grep -v ignore | awk -F' as ' '{print $2}')

if [ "${CIRCLE_BRANCH}" = "" ] ; then
  CIRCLE_BRANCH=master
fi

CACHE_FROM=""
dp () {
  docker pull "$1" || true
  CACHE_FROM="${CACHE_FROM} --cache-from=${1}"
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

docker buildx create --use

build_image () {
    LAYER=$1
    TARGET=""
    TAG="--tag=${IMAGEC}-${LAYER}-${CIRCLE_BRANCH}"
    if [ "x${LAYER}" != "x" ]; then
        TARGET="--target=${LAYER}"
    fi

    # linux/arm64
    docker buildx build --platform=linux/amd64 -o type=image "$TAG" "$TARGET" "$CACHE_FROM" "$ARGS" --file "$FILE" .
}

for i in $LAYERS; do
    build_image "$i"
done

build_image
docker tag "${IMAGEC}--${CIRCLE_BRANCH}" "${IMAGE}:latest"
