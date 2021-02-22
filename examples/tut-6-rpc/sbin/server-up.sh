#! /bin/sh
# Usage: sbin/server-up.sh
# (Run from tut-6-rpc)
set -e

# from examples/tut-6-rpc
ROOT="$(realpath ../..)"

REACH_RPC_KEY=$(cat REACH_RPC_KEY.txt)
export REACH_RPC_KEY

echo
echo 'starting server...'
(cd server && "${ROOT}/reach" server &)
sleep 10
echo 'started...'
echo
