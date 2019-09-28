#!/bin/sh -eu

PORT=30303
RPCPORT=8545

function check_alive() {
    curl -sSf -X POST \
         -H "Content-Type: application/json" \
         --data '{"jsonrpc":"2.0", "method": "web3_clientVersion", "params":[], "id":67}' http://localhost:$RPCPORT
}

if check_alive ; then
    exit 0
fi

geth --allow-insecure-unlock \
     --dev \
     --dev.period=0 \
     --mine \
     --nodiscover \
     --maxpeers 0 \
     --rpc \
     --rpcport $RPCPORT \
     --rpcaddr 0.0.0.0 \
     --rpcapi "db,eth,net,debug,web3,light,personal,admin" \
     --rpccorsdomain '*' \
     --nousb \
     --networkid 17 \
     --nat "any" \
     --ipcpath .ethereum/geth.ipc \
    &
SERVER=$!

while ! check_alive ; do
    echo "Geth not started yet, waiting..."
    sleep 1
done

wait $SERVER
