#!/bin/bash -e

HT="${HOME}/Dev/dist/hub-tool/hub-tool"

for IMG in haskell-build-artifacts reach reach-cli js-deps stdlib runner react-runner rpc-server devnet-algo devnet-eth devnet-cfx ; do
  for TAG in $("${HT}" tag ls "reachsh/${IMG}" --sort updated=desc --format json | jq -r '.[] | .Name' | grep -e ':circleci' -e 'test_dlc') ; do
    if ! (echo "$TAG" | grep master >/dev/null) ; then
      "${HT}" tag rm -f "$TAG"
    fi
  done
done
