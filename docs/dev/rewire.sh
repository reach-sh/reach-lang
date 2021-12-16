#!/bin/sh -e
SRC=../md/
DEST=./src/xxx

L=
r() {
  FROM="${SRC}/${1}.md"
  TOD="${DEST}/${2}"
  TO="${TOD}/index.md"
  mkdir -p "${TOD}"
  mv "${FROM}" "${TO}"
  L="${TOD}"
}
rank() {
  RANK="${1}"
  cat > "${TOD}"/config.json <<EOF
  { "bookRank": ${RANK} }
EOF
}
bookrank() {
  RANK="${1}"
  BOOK="${2}"
  cat > "${TOD}"/config.json <<EOF
  { "bookRank": ${RANK},
    "bookTitle": "${BOOK}"
  }
EOF
}

r reach .

  r overview tut/overview
  rank 10
  r tut tut/rps
  rank 20
    r tut-7-rpc tut/rps/7-rpc
    rank 10

r ref-model model
rank 10

r ref ref
rank 20
  r ref-install ref/install
  rank 10
  r ref-usage ref/usage
  rank 20

r ref-programs reach
rank 30
  r ref-programs-module reach/module
  rank 10
  r ref-programs-appinit reach/appinit
  rank 20
  r ref-programs-step reach/step
  rank 30
  r ref-programs-local reach/local
  rank 40
  r ref-programs-consensus reach/consensus
  rank 50
  r ref-programs-compute reach/compute
  rank 60
  r ref-error-codes reach/errors
  rank 70

r ref-backends compiled
rank 40
  r ref-backends-js compiled/js
  rank 10

r ref-frontends frontend
rank 50
  r ref-frontends-js frontend/js
  rank 10

r ref-networks networks
rank 60

r ref-backends-rpc rpc
bookrank 70 "Reach RPC Server"
  r ref-frontends-rpc-cs rpc/cs
  rank 10
  r ref-frontends-rpc-go rpc/go
  rank 20
  r ref-frontends-rpc-js rpc/js
  rank 30
  r ref-frontends-rpc-py rpc/py
  rank 40
  r ref-backends-rpc-proto rpc/proto
  rank 50
  r ref-backends-rpc-client rpc/client
  rank 60

r workshop workshop
bookrank 80 "Workshops"
  r workshop-hash-lock workshop/hash-lock
  rank 10
  r workshop-relay workshop/relay
  rank 20
  r workshop-trust-fund workshop/trust-fund
  rank 30
  r workshop-fomo workshop/fomo
  rank 40
  r workshop-fomo-generalized workshop/fomo-generalized
  rank 50

r guide guide
bookrank 90 "Guide"
  r guide-windows guide/windows
  rank 10
  r guide-versions guide/versions
  rank 20
  r guide-solidity guide/solidity
  rank 30
  r guide-rpc guide/rpc
  rank 40
  r guide-logging guide/logging
  rank 50
  r guide-nntoks guide/nntoks
  rank 60
  r guide-ctransfers guide/ctransfers
  rank 70
  r guide-assert guide/assert
  rank 80
  r guide-loop-invs guide/loop-invs
  rank 90
  r guide-timeout guide/timeout
  rank 100
  r guide-determ guide/determ
  rank 110
  r guide-race guide/race
  rank 120
  r guide-abstract guide/abstract
  rank 130
  r guide-ganache guide/ganache
  rank 140
  r guide-editor-support guide/editor-support
  rank 150
  r guide-packages guide/packages
  rank 160
  r guide-reach guide/reach
  rank 170
  r guide-limits guide/limits
  rank 180
  r guide-roadmap guide/roadmap
  rank 190

r guide-changelog changelog
rank 100

ls "${SRC}"
exec rmdir "${SRC}"
