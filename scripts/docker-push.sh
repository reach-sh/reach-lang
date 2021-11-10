#!/bin/sh
HERE=$(dirname "$0")
. "${HERE}"/../VERSION

IMAGE="$1"
LATEST_TAG="${IMAGE}:latest"
REACH_GIT_HASH="$("${HERE}/git-hash.sh")"
DATE="$(date '+%Y-%m-%d')"

tagpush() {
    #docker tag "${LATEST_TAG}" "${IMAGE}:$1"
    echo "${LATEST_TAG}" "${IMAGE}:$1"
    if ! [ "${TAG_ONLY}" = "1" ] ; then
      #docker push "${IMAGE}:$1"
      echo "${IMAGE}:$1"

    fi
}

is_rc=$(echo ${VERSION} | grep -e '\-rc\.' > /dev/null ; echo $?)

if [ $is_rc -eq 1 ] ; then
  echo "I WILL PUSH THE TAG latest"
  #tagpush "latest"
  #tagpush "stable"
fi

tagpush $VERSION

#tagpush "${DATE}"
if [ ${#REACH_GIT_HASH} = 8 ]; then
  tagpush "${REACH_GIT_HASH}"
fi

#if [ "${MAJOR}.${MINOR}.${PATCH}" = "${STABLE}" ] ; then
#    tagpush stable
#fi
