#!/bin/sh
HERE=$(dirname "$0")
# shellcheck source=/dev/null
. "${HERE}"/../VERSION

IMAGE="$1"
LATEST_TAG="${IMAGE}:latest"
REACH_GIT_HASH="$("${HERE}/git-hash.sh")"
DATE="$(date '+%Y-%m-%d')"

tagpush() {
    docker tag "${LATEST_TAG}" "${IMAGE}:$1"
    if ! [ "x${TAG_ONLY}" = "x1" ] ; then
      docker push "${IMAGE}:$1"
    fi
}

tagpush "latest"
tagpush "${MAJOR}.${MINOR}.${PATCH}"
tagpush "${MAJOR}.${MINOR}"
tagpush "${MAJOR}"
tagpush "${DATE}"
if [ ${#REACH_GIT_HASH} = 8 ]; then
  tagpush "${REACH_GIT_HASH}"
fi

if [ "${MAJOR}.${MINOR}.${PATCH}" = "${STABLE}" ] ; then
    tagpush stable
fi
