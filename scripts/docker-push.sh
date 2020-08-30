#!/bin/sh
HERE=$(dirname "$0")
# shellcheck source=/dev/null
. "${HERE}"/../VERSION

IMAGE="$1"
LATEST_TAG="${IMAGE}:latest"

docker push "${LATEST_TAG}"

tagpush() {
    docker tag "${LATEST_TAG}" "${IMAGE}:$1"
    if ! [ "x${TAG_ONLY}" = "x1" ] ; then        docker push "${IMAGE}:$1"
    fi
}

tagpush "${MAJOR}.${MINOR}.${PATCH}"
tagpush "${MAJOR}.${MINOR}"
tagpush "${MAJOR}"

if [ "${MAJOR}.${MINOR}.${PATCH}" = "${STABLE}" ] ; then
    tagpush stable
fi
