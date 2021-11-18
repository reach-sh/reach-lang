#!/bin/sh
set -ex

git diff --exit-code || (printf '\n\n\nGit repo is not clean; please commit first.' && exit 1)

IMAGE=reachsh/stdlib:latest
HASH="$(docker run --entrypoint /bin/sh "$IMAGE" -c 'echo $REACH_GIT_HASH')"

rm -f ./*.mjs ./*.d.ts
rm -rf ./dist/
docker run --entrypoint /bin/sh --volume "$(pwd):/cwd" "$IMAGE" \
  -c 'cp /stdlib/*.mjs /stdlib/package.json /cwd/ && cp -r /stdlib/dist /cwd/dist && cp /stdlib/dist/types/* /cwd/'


sudo chown -R $(whoami) ./*

rm ./version.mo.d.ts ./version.mo.mjs

CHANGES=$(git diff --exit-code; echo $?)

if [ $CHANGES -eq 1 ]; then
  git add .
  git config user.name "circleci"
  git config user.email "circleci@reach.com"
  
  git commit -m "'refresh -> reach-sh/reach-lang@$HASH'"
fi