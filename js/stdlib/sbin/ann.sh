#! /bin/sh
set -e

IMAGE=reachsh/stdlib:latest
git_hash="$(docker run --entrypoint /bin/sh "$IMAGE" -c 'echo $REACH_GIT_HASH')"
js_version="$(npm info . version)"
version="$(echo "$js_version" | cut -f 1 -d '-')"

export git_hash
export js_version
export version

mo sbin/ann.txt
