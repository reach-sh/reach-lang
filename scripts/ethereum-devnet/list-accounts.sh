#!/bin/sh

# use "curl" to show accounts (list of addresses) on our test net

exec curl -X POST -H "Content-Type: application/json" --data '{"jsonrpc":"2.0","method":"eth_accounts","params":[],"id":1}' localhost:8545
