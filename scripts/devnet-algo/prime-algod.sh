#!/bin/sh
set -ex

algod -d "${ALGORAND_DATA}" &
PID_ALGO=$!

# time for 1 round, maybe 2
sleep 10

kill "$PID_ALGO" || kill -9 "$PID_ALGO"
