#!/bin/sh
WHICH="$1"

mkdir -p "/tmp/workspace/docker/${WHICH}"
echo "docker save reachsh/${WHICH}:latest..."
docker save "reachsh/${WHICH}:latest" | gzip > "/tmp/workspace/docker/${WHICH}/${WHICH}".tar.gz
