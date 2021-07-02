#!/bin/sh
set -ex

algod -d "${ALGORAND_DATA}" &
PID_ALGO=$!

FAUCET=$(cat "${ALGORAND_DATA}/../FAUCET.address")
DONE=N
while [ "x$DONE" = "xN" ] ; do
  if goal clerk send --from=${FAUCET} --to=${FAUCET} --fee=1000 --amount=0 --note="Reach DevNet Initialized" ; then
    DONE=Y
  fi
done

kill "$PID_ALGO" || kill -9 "$PID_ALGO"
