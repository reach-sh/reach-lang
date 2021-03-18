#! /bin/sh
# Usage: sbin/server-up.sh
# (Run from tut-7-rpc)
set -e

CLIENT="$1"
REACH_RPC_KEY=$(cat REACH_RPC_KEY.txt)
export REACH_RPC_KEY

if [ "x$CLIENT" = "x" ]; then
  echo 'Error: An argument to this script is required'
  exit 1
fi

echo
echo "Running client ${CLIENT}..."
docker-compose -f "docker-compose.yml" up --remove-orphans --force-recreate "client-${CLIENT}-only"
echo 'Ran.'
echo
