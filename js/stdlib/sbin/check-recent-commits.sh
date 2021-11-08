#!/bin/sh
set -ex

git diff --exit-code || (printf '\n\n\nGit repo is not clean; please commit first.' && exit 1)

IMAGE=reachsh/stdlib:latest
HASH="$(docker run --entrypoint /bin/sh "$IMAGE" -c 'echo $REACH_GIT_HASH')"
git log --max-count=10 --pretty="%s" | grep "reach-sh/reach-lang@$HASH" || (printf "\n\nPlease run 'make refresh'" && exit 1)
