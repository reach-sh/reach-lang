#! /bin/sh
# Usage: sbin/server-down.sh
# (Run from tut-7-rpc)

REACH_RPC_KEY=$(cat REACH_RPC_KEY.txt)
export REACH_RPC_KEY

echo
echo 'Stopping server...'
curl --http1.1 -k -H "X-API-Key: $REACH_RPC_KEY" -X POST https://localhost:3000/stop
echo
echo 'Stopped.'
echo

docker rm -f server_ethereum-devnet_1
docker rm -f server_algorand-devnet_1
docker rm -f server_algorand-postgres-db_1
docker network rm server_default
