#! /bin/sh
# Usage: sbin/server-up.sh
# (Run from tut-7-rpc)
set -e

# from examples/tut-7-rpc
ROOT="$(realpath ../..)"

REACH_RPC_KEY=$(cat REACH_RPC_KEY.txt)
export REACH_RPC_KEY

echo
echo 'Starting server...'
cd server
"${ROOT}/reach" rpc-server &
pid=$!
cd ..

sleep 10

if pgrep -P "$pid" >/dev/null; then
  echo 'Server started...'
  echo
else
  exit 1
fi
