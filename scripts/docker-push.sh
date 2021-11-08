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
    if ! [ "${TAG_ONLY}" = "1" ] ; then
      docker push "${IMAGE}:$1"
    fi
}

is_rc=$(echo ${VERSION} | grep rc > /dev/null ; echo $?)

if [ $is_rc -eq 1 ] ; then
  echo "I WILL PUSH THE TAG latest"
fi
#tagpush "latest"
tagpush "${MAJOR}.${MINOR}.${PATCH}"
#tagpush "${DATE}"
if [ ${#REACH_GIT_HASH} = 8 ]; then
  tagpush "${REACH_GIT_HASH}"
fi

if [ "${MAJOR}.${MINOR}.${PATCH}" = "${STABLE}" ] ; then
    tagpush stable
fi
