#! /bin/sh
# Usage: sbin/server-up.sh
# (Run from tut-7-rpc)
set -e

# from examples/tut-7-rpc
ROOT="$(realpath ../..)"

REACH_RPC_KEY=$(cat REACH_RPC_KEY.txt)
export REACH_RPC_KEY

echo
echo 'starting server...'
(cd server && "${ROOT}/reach" rpc-server &)
sleep 10
echo 'started...'
echo
