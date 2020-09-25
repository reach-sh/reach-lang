#!/bin/sh

algod -d "${ALGORAND_DATA}" &
LOG="${ALGORAND_DATA}/node.log"
while ! [ -f "${LOG}" ] ; do sleep 1 ; done
tail -f "${LOG}" | grep compile
