#!/usr/bin/env bash

SAN=reach-server
TLS="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/tls
KEY="$TLS/$SAN.key"
CSR="$TLS/$SAN.csr"
CRT="$TLS/$SAN.crt"

mkdir -p "$TLS"

[ -e "$KEY" ] && rm "$KEY"
[ -e "$CSR" ] && rm "$CSR"
[ -e "$CRT" ] && rm "$CRT"

openssl req \
  -new \
  -subj        "/C=US/CN=$SAN" \
  -addext      "subjectAltName = DNS:$SAN" \
  -newkey      rsa:4096 \
  -keyout      "$KEY" \
  -out         "$CSR"

openssl x509 \
  -req \
  -days        3650 \
  -extensions  SAN \
  -extfile     <(printf "[SAN]\nsubjectAltName = DNS:%s" $SAN) \
  -in          "$CSR" \
  -signkey     "$KEY" \
  -out         "$CRT"
