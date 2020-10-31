#!/bin/sh
WHO="$1"
export REACH_CONNECTOR_MODE=ETH-live
export REACH_DEBUG=1
export ETH_NODE_URI=https://node.offchainlabs.com:8547

DC=docker-compose.offchain.yml
sed -e '31s/ &default-app//' -e '24s/:/: \&default-app/' docker-compose.yml > "${DC}"

make build || exit 1

docker-compose -f "${DC}" up --force-recreate -d "${WHO}" || exit 1
exec docker attach "tut-7_${WHO}_1"
