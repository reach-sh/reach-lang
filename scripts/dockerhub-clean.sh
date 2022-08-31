#!/bin/bash -e

HT="${HOME}/Dev/dist/hub-tool/hub-tool"
FILE=dockerhub

# Delete the old ones
for IMG in reach reach-cli runner react-runner rpc-server devnet-algo devnet-eth devnet-cfx ; do
  for TAG in $("${HT}" tag ls "reachsh/${IMG}" --sort updated=asc --format json | jq -r '.[] | .Name' | grep -e 'circleci') ; do
    "${HT}" tag rm -f "$TAG"
  done
done
exit 0

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

