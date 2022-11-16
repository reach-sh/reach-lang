#!/bin/sh -ex
make build-m1

TMP=/tmp
CNF=$HOME/.config/reach

cd ..
rm $TMP/out.sh

echo Running reach-cli

set +e
docker run -i --rm \
  -e "REACH_EX=$0" \
  -e "REACH_CONNECTOR_MODE" \
  -e "REACH_DEBUG" \
  -e "REACH_IDE" \
  -e "REACH_RPC_KEY" \
  -e "REACH_RPC_PORT"  \
  -e "REACH_RPC_SERVER" \
  -e "REACH_RPC_TLS_CRT" \
  -e "REACH_RPC_TLS_KEY" \
  -e "REACH_RPC_TLS_PASSPHRASE" \
  -e "REACH_RPC_TLS_REJECT_UNVERIFIED" \
  -e "REACH_VERSION" \
  -e "CI" \
  -e "SHELL" \
  -v "$(pwd):/app/src" \
  -v "$TMP:/app/tmp" \
  -v "$CNF:/app/config" \
  -l "sh.reach.dir-project=$(pwd)" \
  -u "$(id -ru):$(id -rg)" \
  --name "reach-cli-$$" \
  --platform linux/arm64 \
  reachsh/reach-cli \
  --dir-project-host="$(pwd)" \
  --dir-tmp-container="/app/tmp" \
  --dir-tmp-host="$TMP" \
  --dir-config-host="$CNF" \
  compile examples/rps-7-loops/index.rsh
set -e

echo Finished reach-cli

echo Running reachc

docker run -i --rm \
  -e "REACH_DEBUG" \
  -v "$(pwd):/app" \
  -v "$TMP:/app/tmp" \
  -v "$CNF:/app/config" \
  -l "sh.reach.dir-project=$(pwd)" \
  -u "$(id -ru):$(id -rg)" \
  --name "reachc-$$" \
  --platform linux/arm64 \
  reachsh/reach \
  examples/rps-7-loops/index.rsh

