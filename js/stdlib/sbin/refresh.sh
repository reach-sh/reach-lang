#!/bin/sh
set -ex

git diff --exit-code || (printf '\n\n\nGit repo is not clean; please commit first.' && exit 1)

IMAGE=reachsh/stdlib:latest
HASH="$(docker run --entrypoint /bin/sh "$IMAGE" -c 'echo $REACH_GIT_HASH')"

rm -f ./*.mjs ./*.d.ts
rm -rf ./dist/
docker run --entrypoint /bin/sh --volume "$(pwd):/cwd" "$IMAGE" \
  -c 'cp /stdlib/*.mjs /stdlib/package.json /cwd/ && cp -r /stdlib/dist /cwd/dist && cp /stdlib/dist/types/* /cwd/'

sudo chown -R "$(whoami)" ./*

if [ -f ./version.mo.d.ts ]; then
  rm -f ./version.mo.d.ts
fi
if [ -f ./version.mo.mjs ]; then
  rm -f ./version.mo.mjs
fi

git diff --exit-code > /dev/null || CHANGES=$?

if [ "${CHANGES}x" = "1x" ]; then
  git add .
  git config user.name "circleci"
  git config user.email "circleci@reach.com"
  git commit -m "'refresh -> reach-sh/reach-lang@$HASH'"
else
  printf '\n\n\nThere are no changes on stdlib. Cannot release.'
fi
