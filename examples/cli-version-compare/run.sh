#!/usr/bin/env bash
set -e

LDIR="$(find . ! -name . -prune -type d -exec sh -c 'echo "$1" | wc -m' shell {} \; | sort -h | tail -n 1)"
ROOT="$( ([ "$REACH_DOCKER" = 0 ] && pwd) || echo /app/src)"

e () {
  printf '\e[34;1mRunning %s...\e[0m' "$2"
  if [ "$REACH_DOCKER" = 0 ]; then
    echo
  fi

  set +e
  o=$(REACH_VERSION="$( ([ -f "./$2/REACH_VERSION" ] && cat "./$2/REACH_VERSION") || echo)" \
    ../../reach version-compare2 \
      --non-interactive \
      --ils="$ROOT/$2/l.json" \
      --stub-remote="$ROOT/$2/r.json" \
      --stub-script="TODO")
  c=$?
  set -e

  e="$(cat "./$2/o.txt")"
  if [ ! $c -eq "$1" ] || [ ! "$o" = "$e" ]; then
    echo

    if [ ! $c -eq "$1" ]; then
      printf '\e[31;1mExpected %s to exit with %s but got %s.\e[0m\n' "$2" "$1" "$c"
    fi

    if [ ! "$o" = "$e" ]; then
      printf '\e[31;1mUnexpected output:\e[0m\n'
      echo "$o" | diff --color "./$2/o.txt" -
    fi

    echo
    exit 1
  fi

  if [ "$REACH_DOCKER" = 0 ]; then
    printf '\e[1;32m ✔\e[0m Exited with %s\n' "$1"
  else
    printf '\e[1;32m  %*s\e[0m Exited with %s' "$((LDIR - ${#2}))" "✔" "$1"
  fi
  echo
}

e  1 unknown-img
e  1 unknown-tag:0.1.7
e  1 unknown-tag:dab71275
e 60 new-images:reach-cli+react-runner+runner
e 60 new-images:rpc-server#new-tags:reach-cli+devnet-cfx
e 60 new-tags:reach+react-runner
e  0 fully-synchronized
e  0 connector-avail:eth+cfx
