#!/bin/sh -e
ROOT=$(pwd)
REACH=${ROOT}/reach

c () {
  echo "c $1"
  ${REACH} compile "$1"
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

jb () {
  # (cd "$ROOT"/js/js-deps && make build)
  (cd "$ROOT"/js/stdlib && make build)
  (cd "$ROOT"/js/runner && make build)
  # (cd "$ROOT"/js && make run)
}

r () {
  printf "\nRun %s\n" "$1"
  (cd "$1"

  ${REACH} clean
  ${REACH} compile --install-pkgs
  ${REACH} compile

  # tealcount1 .

  make build
  # make down

  # jb

  export REACH_DEBUG=1
  REACH_CONNECTOR_MODE=ETH ${REACH} run
  # REACH_CONNECTOR_MODE=CFX ${REACH} run
  # REACH_CONNECTOR_MODE=ALGO ${REACH} run

  # Ganache
  # REACH_CONNECTOR_MODE=ETH-live ETH_NODE_URI=http://host.docker.internal:7545 REACH_ISOLATED_NETWORK=1 ${REACH} run

  # PureStake
  # REACH_CONNECTOR_MODE=ALGO-live ALGO_SERVER=https://testnet-algorand.api.purestake.io/ps2 ALGO_PORT='' ALGO_INDEXER_SERVER=https://testnet-algorand.api.purestake.io/idx2 ALGO_INDEXER_PORT='' ${REACH} run
)
}

tealcount1 () {
  if [ -d "${1}/build" ] ; then
  for t in "${1}"/build/"${2}"*.teal ; do
    (goal clerk compile "${t}" || true)
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

# tealcount

#######

# jb

# c hs/t/n/Err_IllegalEffPosition.rsh
# c hs/t/n/Err_TimeMustBeSimple.rsh
# c hs/t/y/timeout_calc.rsh
# c hs/t/y/tut6-refined.rsh
# c hs/t/n/Err_Apply_ArgCount.rsh
# c hs/t/n/Err_Eval_IncompatibleStates.rsh
# c hs/t/n/Err_Eval_NotObject.rsh
# c hs/t/y/data.rsh
# c hs/t/y/fork.rsh
# c examples/workshop-trust-fund/index.rsh
# c examples/overview/index-error.rsh
# c examples/popularity-contest/index.rsh
# c examples/ttt/index.rsh
# c hs/t/y/array_groups.rsh
# c examples/tut-5/index.rsh
# c examples/nim/index-abstract.rsh
# c examples/timeoutception/index.rsh
# c examples/workshop-trust-fund/index.rsh
# c examples/t/penny.rsh
# c examples/popularity-contest/index.rsh
# c hs/t/y/fork_exp.rsh
# c hs/t/y/pr1011.rsh
# c hs/t/y/lct_in_only.rsh
# c hs/t/y/tut6-refined.rsh
# c hs/t/y/pr20210303.rsh
# c hs/t/y/default_fn_arguments_dependent.rsh
# c hs/t/y/empty_svs.rsh
# c examples/pokechain/index.rsh
# c ../users/reach--tic-tac-toe/V3-megalaser/index.rsh
# c hs/t/y/data.rsh
# c hs/t/y/fork.rsh
# c hs/t/n/Err_NoImpureCondInStep.rsh
# c hs/t/y/array_length.rsh
# c hs/t/y/lhs_array_spread.rsh
# c hs/t/y/pr20210311.rsh
# c hs/t/n/class_addr.rsh
# c hs/t/y/pr1165.rsh
# c hs/t/y/1202.rsh
# c hs/t/y/rest-empty.rsh
# c hs/t/y/rest-tuple.rsh
# c hs/t/y/rest_parameter.rsh
# c hs/t/y/pr138.rsh
# c hs/t/y/adapt_tuple.rsh
# c hs/t/y/tuple-parts.rsh
# c hs/t/y/no-tail-if.rsh
# c hs/t/n/interact_nonlocal.rsh
# c hs/t/y/pr-b4469.rsh
# c hs/t/y/pr-88b8c.rsh
# c hs/t/y/pr-149.rsh
# c hs/t/y/pr-e017b.rsh
# c hs/t/y/pr-202105281833.rsh
# c hs/t/y/map-mt.rsh
# fc hs/t/n/map-dset.rsh
# c hs/t/y/lazy-ifs.rsh
# c hs/t/y/pr-20210603.rsh
# c hs/t/y/pr-3e579.rsh
# c hs/t/y/pr179.rsh
# c hs/t/y/pr-671006.rsh
# fc hs/t/n/pr-671006p.rsh
# c hs/t/y/pr190.rsh
# c hs/t/y/pr190b.rsh

# c hs/t/y/big-d8cff.rsh
# tealcount1 hs/t/y big-d8cff

# err Err_Pay_DoubleToken
# err Err_Pay_DoubleNetworkToken
# err Err_Pay_Type
# err Err_Token_NotOnFirst
# err Err_Token_OnCtor
# err Err_Token_InWhile
# err export_ret_ty
# err Err_Part_DuplicatePart
# err Err_View_UDFun

# r examples/pkg
# r examples/zbeq
# r examples/nim
# c examples/nim/index-abstract.rsh
# r examples/secured-loan
# r examples/overview
# r examples/log
# r examples/argz
# r examples/atomic-swap
# r examples/atomic-swap-auction
# r examples/chicken-fork
# r examples/chicken-parallel
# r examples/chicken-race
# r examples/exports
# r examples/many-args
# r examples/ttt
# r examples/map-sender
# r examples/map-any
# r examples/map-vary
# r examples/map-rwrw # XXX
# r examples/map-multi
# r examples/map-big
# r examples/maybe-send
# r examples/multiple-pr-case
# r examples/multisig
# r examples/nft-auction
# r examples/nft-dumb
# r examples/nim
# r examples/overview
# r examples/popularity-contest
# r examples/pr202105-zet
# r examples/race
# r examples/remote
# r examples/secured-loan
# r examples/timeoutception
# r examples/tut-3
# r examples/tut-4
# r examples/tut-5
# r examples/tut-6
# r examples/tut-5-attack
# r examples/tut-7
# r examples/tut-8
# r examples/tut-7-array
# r examples/variable-transfers
# r examples/view-bytes
# r examples/view-fun
# r examples/view-maybe
# r examples/view-map
# r examples/view-steps
# r examples/weird-swap
# r examples/workshop-fomo
# r examples/workshop-fomo-generalized
# r examples/workshop-hash-lock
# r examples/workshop-relay
# r examples/workshop-trust-fund
# r users/kwame20210311
# r examples/raffle
# r examples/rent-seeking
# r examples/pr-1cc66

r examples/mint-basic

# (cd examples/abstract-simul && make build)

# (cd hs && mk hs-test)
