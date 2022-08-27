#!/bin/bash -e

HT="${HOME}/Dev/dist/hub-tool/hub-tool"

while IFS= read -r TAG; do
    "${HT}" tag rm -f "$TAG"
    sleep 5
done < dockerhub-circleci.txt

exit 0

# Grab all of them
for IMG in reach reach-cli runner react-runner rpc-server devnet-algo devnet-eth devnet-cfx ; do
  "${HT}" tag ls "reachsh/${IMG}" --sort updated=desc --format json --all | jq -r '.[] | .Name' > dockerhub.${IMG}.txt
done
exit 0

# Delete the old ones
while sleep 60 ; do
for IMG in reach reach-cli runner react-runner rpc-server devnet-algo devnet-eth devnet-cfx ; do
  for TAG in $("${HT}" tag ls "reachsh/${IMG}" --sort updated=desc --format json | jq -r '.[] | .Name' | grep -e ':circleci' -e 'test_dlc') ; do
    if ! (echo "$TAG" | grep master >/dev/null) ; then
      "${HT}" tag rm -f "$TAG"
    fi
  done
done
done
