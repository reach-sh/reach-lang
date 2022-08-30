#!/bin/bash -e

HT="${HOME}/Dev/dist/hub-tool/hub-tool"
FILE=dockerhub

# Grab all of them
echo -n > "${FILE}"
for IMG in reach reach-cli runner react-runner rpc-server devnet-algo devnet-eth devnet-cfx ; do
  "${HT}" tag ls "reachsh/${IMG}" --sort updated=desc --format json --all >> "${FILE}.${IMG}.json"
done
exit 0

# Delete from file
while IFS= read -r TAG; do
    "${HT}" tag rm -f "$TAG"
    sleep 5
done < "${FILE}.rm"
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
