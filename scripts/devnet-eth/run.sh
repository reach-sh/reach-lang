#!/bin/sh -xeu

MINIT=${1:-noinit}

# PORT=30303  # unused?
RPCPORT=8545

geth --allow-insecure-unlock \
     --dev \
     --dev.period 0 \
     --mine \
     --miner.noverify \
     --nodiscover \
     --maxpeers 0 \
     --lightkdf \
     --ipcdisable \
     --http \
     --http.port $RPCPORT \
     --http.addr 0.0.0.0 \
     --http.api "eth,net,debug,web3,personal,admin" \
     --http.corsdomain '*' \
     --http.vhosts '*' \
     --nousb \
     --networkid 17 \
     --nat "any" &
PROC=$!

if [ "x${MINIT}" = "xinit" ] ; then
    sleep 2
    exit 0
else
    wait $PROC
fi
