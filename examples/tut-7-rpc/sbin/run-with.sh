#! /bin/sh
# Usage: sbin/server-up.sh
# (Run from tut-7-rpc)
set -e

CLIENT="$1"
CONTAINER="tut-7-rpc_client-${CLIENT}-only_1"
REACH_RPC_KEY=$(cat REACH_RPC_KEY.txt)
export REACH_RPC_KEY

if [ "$CLIENT" = "" ]; then
  echo 'Error: An argument to this script is required'
  exit 1
fi

if docker container inspect "$CONTAINER" >/dev/null 2>&1; then
  docker rm -f "$CONTAINER"
  echo
fi

echo "Running client ${CLIENT}..."
docker-compose -f "docker-compose.yml" up --remove-orphans --force-recreate "client-${CLIENT}-only"
echo "Client ${CLIENT} completed."
echo
