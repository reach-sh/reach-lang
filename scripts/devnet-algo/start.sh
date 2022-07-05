#!/bin/bash -x

# start daily ping telemetry script
sh /daily-ping.sh &

# Start postgres
export PGDATA="$POSTGRES_DATA"
su postgres -c "postgres" &
bash /wait-for-postgres.sh

if [ "$REACH_DEBUG" = "" ] ; then
  echo Not starting debugger. To start, use REACH_DEBUG=1.
else
  echo Starting debugger
  mkdir -p /dbg
  export TEAL_DEBUGGER_DIR=/dbg
  (cd /dbg || exit 1;
   while sleep 90; do
      /rotate_dbg.sh 16
   done) &
fi

# Disable telemetry
export ALGOTEST=Y

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
  --postgres "host=localhost port=${POSTGRES_PORT} user=${POSTGRES_USER} password=${POSTGRES_PASSWORD} dbname=${POSTGRES_DB} sslmode=disable"
 echo Indexer died, restarting...
 done) 1>"${ILOG}" 2>&1 &

while ! [ -f "${ALOG}" ] || ! [ -f "${ILOG}" ]; do
  echo Waiting for logs...
  sleep 1
done
tail -f "${ALOG}" | sed 's/^/ALGOD:/' &
tail -f "${ILOG}" | sed 's/^/INDEX:/'
