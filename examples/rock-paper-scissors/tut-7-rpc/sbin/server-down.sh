#! /bin/sh
# Usage: sbin/server-down.sh
# (Run from tut-7-rpc)

REACH_RPC_KEY=$(cat REACH_RPC_KEY.txt)
export REACH_RPC_KEY

echo 'Stopping server...'
curl --http1.1 -sk -o /dev/null -H "X-API-Key: $REACH_RPC_KEY" -X POST https://localhost:3000/stop
echo 'Stopped.'
echo
