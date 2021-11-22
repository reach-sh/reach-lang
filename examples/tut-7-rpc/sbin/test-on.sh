#! /bin/sh
# Usage: sbin/test-on.sh REACH_CONNECTOR_MODE CLIENT1 CLIENT2 ...
# (Run from tut-7-rpc)
set -e

export REACH_CONNECTOR_MODE="$1"
shift

sbin/server-up.sh
for CLIENT in "$@"; do
  sbin/run-with.sh "$CLIENT"
done
sbin/server-down.sh
