#!/bin/sh
IMAGE_NAME=$1
LAYERS=$2
DOCKER_FILE=$3
shift 3
ARGS=$@

CACHE_FROM="--cache-from=${IMAGE_NAME}:${CIRCLE_BRANCH}"

for i in $LAYERS; do
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

    #echo "PWD: $(pwd)"
    #echo "FILES ON DIR: $(ls -la .)"
    echo docker build $TAG $TARGET $CACHE_FROM $ARGS --file $DOCKER_FILE .
    echo " "
}

for i in $LAYERS; do
    build_image $i
done

build_image