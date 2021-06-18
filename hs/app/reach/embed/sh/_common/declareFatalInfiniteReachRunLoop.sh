# shellcheck disable=SC2016
fatal_infinite_reach_run_loop () {
  echo 'reach run has detected an infinite loop'
  echo '`make run` may not call `reach run` with no arguments'
  echo 'instead try something like `reach run index` from `make run`'
  exit 1
}
