#!/bin/sh -eu

PORT=30303
RPCPORT=8545

PREFUNDER_ACCOUNT=${PREFUNDER_ACCOUNT:?"PREFUNDER_ACCOUNT must be set"}
PREFUNDER_ENC_KEY=${PREFUNDER_ENC_KEY:?"PREFUNDER_ENC_KEY must be set"}

# PREFUNDER_AMOUNT=${PREFUNDER_AMOUNT:?"PREFUNDER_AMOUNT must be set"}

# sed -i s/__PREFUNDER_ACCOUNT__/$PREFUNDER_ACCOUNT/g genesis.json
# sed -i s/__PREFUNDER_AMOUNT__/$PREFUNDER_AMOUNT/g genesis.json

# geth --dev init genesis.json

mkdir -p datadir/keystore/
echo $PREFUNDER_ENC_KEY > datadir/keystore/key.json

geth --allow-insecure-unlock \
     --datadir datadir \
     --miner.etherbase $PREFUNDER_ACCOUNT \
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
