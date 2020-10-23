#!/bin/sh -x

rm -fr local debug
cp -fr algorand_network local
cp -fr algorand_data/* local/Primary/

clean() {
  (killall -9 tealdbg || exit 0)
  (killall -9 algod || exit 0)
}

clean

export POSTGRES_USER=algogrand
export POSTGRES_PASSWORD=indexer
export POSTGRES_DB=pgdb

(docker container stop algopgdb || exit 0)
docker run -d --name algopgdb --rm -e POSTGRES_USER -e POSTGRES_PASSWORD -e POSTGRES_DB -p 5432:5432 postgres:11
sleep 1

export TEAL_DEBUGGER_URL=http://localhost:9392

export POSTGRES_HOST=localhost
ALGORAND_DATA=$(pwd)/local/Primary
export ALGORAND_DATA
START=$(pwd)/start.sh
[ -d ../../../algorand/indexer ] || (echo 'please clone algorand/indexer in the right place' && exit 1)
export PATH=../../../algorand/indexer/cmd/algorand-indexer:${PATH}
${START} || exit 0

clean
