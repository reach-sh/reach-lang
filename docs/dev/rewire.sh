#!/bin/sh -e
SRC=../md/
DEST=./src/en/md

r() {
  FROM="${SRC}/${1}.md"
  TO="${DEST}/${2}/index.md"
  mkdir -p "$(dirname "${TO}")"
  mv "${FROM}" "${TO}"
}

r guide guide
r guide-abstract guide/abstract
r guide-assert guide/assert
r guide-changelog changelog
r guide-ctransfers guide/ctransfers
r guide-determ guide/determ
r guide-editor-support guide/editor-support
r guide-ganache guide/ganache
r guide-limits guide/limits
r guide-logging guide/logging
r guide-loop-invs guide/loop-invs
r guide-nntoks guide/nntoks
r guide-packages guide/packages
r guide-race guide/race
r guide-reach guide/reach
r guide-roadmap guide/roadmap
r guide-rpc guide/rpc
r guide-solidity guide/solidity
r guide-timeout guide/timeout
r guide-versions guide/versions
r guide-windows guide/windows
r overview tut/overview
r reach top
r ref ref
r ref-backends compiled
r ref-backends-js compiled/js
r ref-backends-rpc rpc
r ref-backends-rpc-client rpc/client
r ref-backends-rpc-proto rpc/proto
r ref-error-codes reach/errors
r ref-frontends frontend
r ref-frontends-js frontend/js
r ref-frontends-rpc-cs rpc/cs
r ref-frontends-rpc-go rpc/go
r ref-frontends-rpc-js rpc/js
r ref-frontends-rpc-py rpc/py
r ref-model model
r ref-networks networks
r ref-programs reach/programs
r ref-programs-appinit reach/appinit
r ref-programs-compute reach/compute
r ref-programs-consensus reach/consensus
r ref-programs-local reach/local
r ref-programs-module reach/module
r ref-programs-step reach/step
r tut tut/rps
r tut-7-rpc tut/rps/7-rpc
r workshop workshop
r workshop-fomo workshop/fomo
r workshop-fomo-generalized workshop/fomo-generalized
r workshop-hash-lock workshop/hash-lock
r workshop-relay workshop/relay
r workshop-trust-fund workshop/trust-fund

ls "${SRC}"
exec rmdir "${SRC}"
