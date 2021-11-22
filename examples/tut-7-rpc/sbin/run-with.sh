#! /bin/sh
# Usage: sbin/server-up.sh
# (Run from tut-7-rpc)
set -e

CLIENT="$1"

REACH_RPC_PORT=3000
REACH_RPC_TLS_REJECT_UNVERIFIED=0
REACH_RPC_KEY=$(cat REACH_RPC_KEY.txt)

export REACH_RPC_PORT REACH_RPC_TLS_REJECT_UNVERIFIED REACH_RPC_KEY

if [ "$CLIENT" = "" ]; then
  echo 'Error: An argument to this script is required'
  exit 1
fi

echo "Running client ${CLIENT}..."
REACH_RPC_SERVER="127.0.0.1" ../../reach rpc-server-await

cd "client-$CLIENT"
docker run --rm \
  -e         "REACH_DEBUG" \
  -e         "REACH_RPC_SERVER=host.docker.internal" \
  -e         "REACH_RPC_PORT" \
  -e         "REACH_RPC_TLS_REJECT_UNVERIFIED" \
  -e         "REACH_RPC_KEY" \
  --add-host "host.docker.internal:172.17.0.1" \
  --name     "reach-app-rpc-client-$CLIENT" \
  --network  reach-devnet \
  "reachsh/reach-app-tut-7-rpc-client-$CLIENT:latest"

echo "Client ${CLIENT} completed."
echo
