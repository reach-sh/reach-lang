#!/bin/sh
set -ex

git diff --exit-code || (printf '\n\n\nGit repo is not clean; please commit first.' && exit 1)

IMAGE=reachsh/stdlib:latest
HASH="$(docker run --entrypoint /bin/sh "$IMAGE" -c 'echo $REACH_GIT_HASH')"
#if [ $(echo "$HASH" | wc -c) -eq 9 ]; then
#  :
#else
#  set +x
#  echo
#  echo
#  echo "Hash length looks wrong: ${HASH}"
#  echo '(A trailing asterisk means the image was made from a dirty git repo)'
#  echo
#  exit 1
#fi

rm -f ./*.mjs ./*.d.ts
rm -rf ./dist/
docker run --entrypoint /bin/sh --volume "$(pwd):/cwd" "$IMAGE" \
  -c 'cp /stdlib/*.mjs /stdlib/package.json /cwd/ && cp -r /stdlib/dist /cwd/dist && cp /stdlib/dist/types/* /cwd/'
  # -c 'cp /stdlib/*.mjs /stdlib/package.json /stdlib/*.d.ts /cwd/'


sudo chown -R $(whoami) ./*
# rm tester.mjs ETH-test.mjs
rm ./version.mo.d.ts ./version.mo.mjs

git add .
git config user.name "circleci"
git config user.email "circleci@reach.com"

git commit -m "'refresh -> reach-sh/reach-lang@$HASH'"