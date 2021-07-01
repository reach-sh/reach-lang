#!/bin/sh
set -e

HERE=$(dirname "$0")
# shellcheck source=/dev/null
. "${HERE}"/../VERSION

FAILED=false
# reach, devnet-algo omitted because CircleCI doesn't build them yet
for NAME in stdlib runner ethereum-devnet ; do
  IMAGE="reachsh/$NAME:$VERSION"
  # if you build it locally, it doesn't have .RepoDigests
  LOCAL_SHA="$(docker inspect --format='{{.Id}}' "$IMAGE")"
  HUB_SHA="$(curl -s "https://hub.docker.com/v2/repositories/reachsh/$NAME/tags/$VERSION" | jq -r '.images[0].digest')"
  if ! [ "x$LOCAL_SHA" = "x$HUB_SHA" ] ; then
    echo "
$IMAGE
LOCAL: $LOCAL_SHA
HUB:   $HUB_SHA
shas do not match, this may indicate that you need to push
=== XXX WARNING: this script is known to be broken XXX ===
"
    FAILED=true
  fi
done

if [ $FAILED ] ; then
  exit 1
fi
