#!/bin/bash -x

POSTGRES_PORT=5432

# shellcheck disable=SC2153
echo "Wait for $POSTGRES_HOST:$POSTGRES_PORT"
# shellcheck disable=SC2188
while ! <"/dev/tcp/$POSTGRES_HOST/$POSTGRES_PORT"; do
  echo not ready yet, trying again in 1s...
  sleep 1
done

if [ "x$REACH_DEBUG" = "x" ] ; then
  echo Not starting debugger. To start, use REACH_DEBUG=1.
else
  echo Starting debugger
  mkdir -p /dbg
  export TEAL_DEBUGGER_DIR=/dbg
fi

echo Starting algod
algod -d "${ALGORAND_DATA}" &
PID_ALGO=$!

echo Checking for algod.net
while ! [ -f "${ALGORAND_DATA}/algod.net" ] ; do
  echo "Waiting to start indexer..."
  sleep 1
done

echo Starting indexer
touch algorand-indexer.yaml
algorand-indexer daemon \
  --algod "${ALGORAND_DATA}" \
  --pidfile "${ALGORAND_DATA}/indexer.pid" \
  --dev-mode \
  --token "reach-devnet" \
  --postgres "host=${POSTGRES_HOST} port=${POSTGRES_PORT} user=${POSTGRES_USER} password=${POSTGRES_PASSWORD} dbname=${POSTGRES_DB} sslmode=disable" &
PID_IDX=$!

LOG="${ALGORAND_DATA}/node.log"
while ! [ -f "${LOG}" ] ; do sleep 1 ; done
tail -f "${LOG}"

kill "$PID_ALGO" "$PID_IDX"
kill -9 "$PID_ALGO" "$PID_IDX"
