#!/bin/sh -x

rm -fr local
cp -fr algorand_network local
cp -fr algorand_data local/Primary

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

export TEAL_DEBUGGER_URL=http://localhost:9392

export POSTGRES_HOST=localhost
export ALGORAND_DATA=$(pwd)/local/Primary
START=$(pwd)/start.sh
cd ../../../algorand/indexer
${START} || exit 0

clean
