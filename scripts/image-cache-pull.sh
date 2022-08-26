#!/bin/bash -xe
for IMAGE in "$@"; do
  if [ "$IMAGE" != "" ]; then
    IMAGEC="${REGISTRYC}/${IMAGE}:circleci"
    TAGC="${IMAGEC}-${CIRCLE_SHA1}"

    TRIES=0
    while ! docker pull "${TAGC}" && ((TRIES < 5)); do
      sleep 3
      TRIES=$((TRIES + 1))
    done
    docker tag "${TAGC}" "reachsh/${IMAGE}:latest"
    docker tag "${TAGC}" "reachsh/${IMAGE}:${VERSION}"
  fi
done
