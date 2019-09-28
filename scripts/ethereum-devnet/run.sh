#!/bin/sh -eu

PORT=30303
RPCPORT=8545

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
     --rpcvhosts '*' \
     --nousb \
     --networkid 17 \
     --nat "any"
