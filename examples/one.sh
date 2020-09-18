#!/bin/sh
set -e

MODE="$1"
e="$2"

has_target() {
  set +e
  make -q "$MODE" 2> /dev/null
  RESULT=$?
  set -e
  [ "$RESULT" -eq 0 ] || [ "$RESULT" -eq 1 ]
}

echo "$e"
(
  cd "$e" || exit 1
  if [ -f Makefile ] && has_target ; then
    set +e
    make "$MODE"
    RESULT=$?
    set -e
    if [ "$MODE" = "run" ] ; then
      set +e
      make down
      set -e
    fi
    if [ "$RESULT" -ne 0 ] ; then
      echo "$e" "$MODE" failed
      exit 1
    fi
    ( exit 0 )
  else
    case "$MODE" in
      build)
        ../../reach compile
        ;;
      run)
        ../../reach run
        ;;
      clean)
        ../../reach clean
        ;;
    esac
  fi
) || exit 1
