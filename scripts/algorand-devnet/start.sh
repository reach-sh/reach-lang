#!/bin/sh

algod -d "${ALGORAND_DATA}" &

touch algorand-indexer.yaml
./cmd/algorand-indexer/algorand-indexer daemon \
  --algod "${ALGORAND_DATA}" \
  --pidfile "${ALGORAND_DATA}/indexer.pid" \
  --dev-mode \
  --token "reach-devnet" \
  --postgres "host=algorand-postgres-db port=5432 user=${POSTGRES_USER} password=${POSTGRES_PASSWORD} dbname=${POSTGRES_DB} sslmode=disable"

# LOG="${ALGORAND_DATA}/node.log"
# while ! [ -f "${LOG}" ] ; do sleep 1 ; done
# tail -f "${LOG}" | grep compile
