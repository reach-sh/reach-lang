#!/bin/sh -e
ROOT=$(pwd)
export REACH="${ROOT}/reach"

export REACH_DOCKER=0
${REACH} -h

# Call like... hta "$@"
ht () {
  cd hs && HS_TEST_ARGS="-p $*" make hs-test
}
hta () {
  cd hs && HS_TEST_ARGS="-p $*" make hs-test-accept
}

gf () {
  echo gf "$@"
  "${ROOT}"/scripts/gen-test-output.sh "$@"
}


c () {
  echo c "$@"
  ${REACH} compile --intermediate-files "$@"
}

ce () {
  c "examples/${1}/index.rsh"
}

fc () {
  if ! [ -f "$1" ] ; then
    echo "$1" does not exist
    false
  fi
  if (c "$1") ; then
    false
  else
    true
  fi
}

err () {
  fc "hs/t/n/$1.rsh"
}

jbi() {
  WHICH="${1}"
  DIR="${ROOT}${WHICH}"
  (cd "${DIR}" && make build)
}

jb () {
  jbi /js/js-deps
  jbi /js/stdlib
  jbi /js/runner
  #jbi /js/rpc-server
  #jbi /js/react-runner
  #jbi /js
}

one () {
  MODE="$1"
  WHICH="$2"
  printf "\nONE %s %s\n" "$MODE" "$WHICH"
  (cd "examples"

  ./one.sh clean "${WHICH}"
  ./one.sh build "${WHICH}"
  REACH_DEBUG=1 REACH_CONNECTOR_MODE="${MODE}" ./one.sh run "${WHICH}"
)
}

ci () {
  MODE="$1"
  WHICH="$2"
  printf "\nCI %s %s\n" "$MODE" "$WHICH"
  (cd "examples/$WHICH"
  ${REACH} clean
  ${REACH} compile --install-pkgs
  ${REACH} compile --intermediate-files
  make build
  REACH_DEBUG=Y REACH_CONNECTOR_MODE="$MODE" ${REACH} run
)
}

r () {
  printf "\nRun %s\n" "$1"
  (cd "$1"

  ${REACH} clean
  rm -f build/*.teal*
  ${REACH} compile --install-pkgs
  ${REACH} compile --intermediate-files

  make build

  #REACH_CONNECTOR_MODE=ETH ${REACH} run
  #REACH_DEBUG=N REACH_CONNECTOR_MODE=ALGO ${REACH} run
  #REACH_CONNECTOR_MODE=CFX ${REACH} run

)
}

ganache () {
  (cd "$1"

   REACH_DEBUG=1 REACH_CONNECTOR_MODE=ETH-live ETH_NODE_URI=http://172.17.0.1:7545 REACH_ISOLATED_NETWORK=1 ${REACH} run
 )}

tealcount1 () {
  if [ -d "${1}/build" ] ; then
  for t in "${1}"/build/"${2}"*.teal ; do
    td=$(dirname "${t}")
    tn=$(basename "${t}")
    (cd "${td}" && ("${ROOT}/scripts/goal-devnet" clerk compile "${tn}" || true))
  done
  SIZE=$(wc -c "${1}"/build/"${2}"*tok | tail -1 | awk '{print $1}')
  echo "$1" "$2" - "$SIZE"
  wc -c "${1}"/build/"${2}"*tok
  fi
}

tealcount () {
  for e in examples/* ; do
    tealcount1 "$e"
  done
}

checkteal () {
  c "$1"/index.rsh
  ./scripts/goal-devnet clerk compile "$1"/build/index.main.appApproval.teal
}

# tealcount

cdot () {
  REACH_DEBUG=Y c "$1"
  dot -Tpng -O "$(dirname "$1")/build/$(basename "$1" .rsh).main.appApproval.dot"
  dot -Tpng -O "$(dirname "$1")/build/$(basename "$1" .rsh).main.state.dot"
}

# Disable this if you want to download a cache from CI (useful when js-deps
# changes)
export REACH_BUILD_NO_CACHE=Y

