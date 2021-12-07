#!/bin/sh
IMAGE_NAME=$1
LAYERS=$2
DOCKER_FILE=$3
shift 3
ARGS=$@

docker pull $IMAGE_NAME:${CIRCLE_BRANCH} || true
docker pull $IMAGE_NAME:master | true

CACHE_FROM="--cache-from=${IMAGE_NAME}:${CIRCLE_BRANCH} --cache-from=${IMAGE_NAME}:master"

for i in $LAYERS; do
    docker pull $IMAGE_NAME:${i}-${CIRCLE_BRANCH} || true
    docker pull $IMAGE_NAME:${i}-master | true
	CACHE_FROM="--cache-from=${IMAGE_NAME}:${i}-${CIRCLE_BRANCH} --cache-from=${IMAGE_NAME}:${i}-master ${CACHE_FROM}";
done

build_image () {
    LAYER=$1
    if [ "x${LAYER}" != "x" ]; then
        TAG="--tag=${IMAGE_NAME}:${LAYER}"
        TARGET="--target=${LAYER}"
    else
        TAG="--tag=${IMAGE_NAME}"
        TARGET=""
    fi

    docker build $TAG $TARGET $CACHE_FROM $ARGS --file $DOCKER_FILE .
}

for i in $LAYERS; do
    build_image $i
done

build_image