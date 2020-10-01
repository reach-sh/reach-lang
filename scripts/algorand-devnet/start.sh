#!/bin/sh -x

echo Starting debugger
racket server.rkt &

export TEAL_DEBUGGER_URL=http://localhost:9392

echo Starting algod
algod -d "${ALGORAND_DATA}" &

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
  --postgres "host=${POSTGRES_HOST} port=5432 user=${POSTGRES_USER} password=${POSTGRES_PASSWORD} dbname=${POSTGRES_DB} sslmode=disable"

# LOG="${ALGORAND_DATA}/node.log"
# while ! [ -f "${LOG}" ] ; do sleep 1 ; done
# tail -f "${LOG}" | grep compile
