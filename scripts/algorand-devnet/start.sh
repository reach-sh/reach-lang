#!/bin/bash -x

POSTGRES_PORT=5432

# shellcheck disable=SC2153
echo "Wait for $POSTGRES_HOST:$POSTGRES_PORT"
# shellcheck disable=SC2188
while ! <"/dev/tcp/$POSTGRES_HOST/$POSTGRES_PORT"; do
  echo not ready yet, trying again in 1s...
  sleep 1
done

if [ "x$REACH_ALGO_DEBUG" = "x" ] ; then
  echo Not starting debugger. To start, use REACH_ALGO_DEBUG=1.
else
  echo Starting debugger
  mkdir -p /dbg
  export TEAL_DEBUGGER_DIR=/dbg
fi

echo Starting algod
ALOG="${ALGORAND_DATA}/node.log"
(while true ; do
 algod -d "${ALGORAND_DATA}"
 echo Algod died, restarting...
done) &

echo Checking for algod.net
while ! [ -f "${ALGORAND_DATA}/algod.net" ] ; do
  echo "Waiting to start indexer..."
  sleep 1
done

echo Starting indexer
touch algorand-indexer.yaml
ILOG="${ALGORAND_DATA}/indexer.log"
(while true ; do
 algorand-indexer daemon \
  --algod "${ALGORAND_DATA}" \
  --pidfile "${ALGORAND_DATA}/indexer.pid" \
  --dev-mode \
  --token "reach-devnet" \
  --postgres "host=${POSTGRES_HOST} port=${POSTGRES_PORT} user=${POSTGRES_USER} password=${POSTGRES_PASSWORD} dbname=${POSTGRES_DB} sslmode=disable"
 echo Indexer died, restarting...
 done) 1>"${ILOG}" 2>&1 &

while ! [ -f "${ALOG}" ] || ! [ -f "${ILOG}" ]; do
  echo Waiting for logs...
  sleep 1
done
tail -f "${ALOG}" | sed 's/^/ALGOD:/' &
tail -f "${ILOG}" | sed 's/^/INDEX:/'
