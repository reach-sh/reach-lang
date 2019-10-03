#!/bin/sh

URI=${ETH_NODE_URI:=http://localhost:8545}

while ! curl -sSf -X POST \
        -H "Content-Type: application/json" \
        --data '{"jsonrpc":"2.0", "method": "web3_clientVersion", "params":[], "id":67}' ${URI} ; do
    sleep 1
done
