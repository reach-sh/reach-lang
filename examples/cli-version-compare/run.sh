#!/usr/bin/env bash
# shellcheck disable=SC2000
set -e

DIRS="$(find . ! -name . -prune -type d | sed 's/^\.\///' | sort -h)"
LDIR="$(for d in $DIRS; do echo "$d" | wc -m; done | sort -h | tail -n 1)"
ROOT="$( ([ "$REACH_DOCKER" = 0 ] && pwd) || echo /app/src)"

e () {
  printf '\e[34;1mRunning %s...\e[0m' "$1"
  if [ "$REACH_DOCKER" = 0 ]; then
    echo
  fi

  set +e
  o=$(REACH_VERSION="$( ([ -f "./$1/REACH_VERSION" ] && cat "./$1/REACH_VERSION") || echo)" \
    ../../reach version-compare2 \
      --non-interactive \
      --ils="$ROOT/$1/l.json" \
      --stub-remote="$ROOT/$1/r.json" \
      --stub-script="./$1/reach") # skip $ROOT because `diff` is evaluated outside of Docker
  c=$?
  set -e

  n="$(cat "./$1/exit")"
  x="$(cat "./$1/o.txt")"
  if [ ! $c -eq "$n" ] || [ ! "$o" = "$x" ]; then
    echo

    if [ ! $c -eq "$n" ]; then
      printf '\e[31;1mExpected %s to exit with %s but got %s.\e[0m\n' "$1" "$n" "$c"
    fi

    if [ ! "$o" = "$x" ]; then
      printf '\e[31;1mUnexpected output:\e[0m\n'
      echo "$o" | diff --color "./$1/o.txt" -
    fi

    echo
    exit 1
  fi

  if [ "$REACH_DOCKER" = 0 ]; then
    printf '\e[1;32m ✔\e[0m Exited with %s\n' "$n"
  else
    printf '\e[1;32m  %*s\e[0m Exited with %s' "$((LDIR - ${#1} + 2))" "✔" "$n"
  fi
  echo
}

for d in $DIRS; do e "$d"; done
