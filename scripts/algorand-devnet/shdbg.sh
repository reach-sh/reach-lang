#!/bin/bash
i=0
RESPONSE="HTTP/1.1 200 OK\r\nConnection: close\r\n\r\nOK\r\n"
while { echo -en "$RESPONSE"; } | nc -lN "${1:-9392}" > "${i}.json"; do
  i=$((i+1))
  echo "================================================"
done
