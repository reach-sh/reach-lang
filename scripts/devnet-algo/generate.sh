#!/bin/sh
set -x

NET_DIR=algorand_network
NET_PRIMARY_NODE_DIR="$NET_DIR/Primary"
FAUCET_ADDRESS_FILE="$NET_DIR/FAUCET.address"
FAUCET_MNEMONIC_FILE="$NET_DIR/FAUCET.mnemonic"

# Generate most of the stuff based on the template
goal \
  network create \
  -r "$NET_DIR" \
  -n "devnet" \
  -t "network_template.json"

goal account list -d "$NET_PRIMARY_NODE_DIR" \
  | awk '{print $2}' > "$FAUCET_ADDRESS_FILE"

# This assumes the output is:
# Exported key for account BLAH: "mnemonic in here"
# (cut -f 6- -d 2 removes the first 5 words.)

# shellcheck disable=SC2002
cat "$FAUCET_ADDRESS_FILE" \
  | xargs goal account export -d "$NET_PRIMARY_NODE_DIR" -a \
  | cut -f 6- -d ' ' \
  | xargs echo > "$FAUCET_MNEMONIC_FILE"
