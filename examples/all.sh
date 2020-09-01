#!/bin/sh

MODE="$1"
EXAMPLES=$(find . -mindepth 1 -maxdepth 1 -type d | sort)

has_target() {
    make -q "$MODE"
    RESULT=$?
    [ "$RESULT" -eq 0 ] || [ "$RESULT" -eq 1 ]
}

for e in $EXAMPLES ; do
    echo "$e"
    (cd "$e" || exit 0
     if [ -f Makefile ] && has_target ; then
         make "$MODE"
         RESULT=$?
         if [ "$MODE" = "build" ] && [ "$RESULT" -ne 0 ] ; then
             echo "$e" build failed
             exit 1
         fi
         if [ "$MODE" = "run" ] ; then
             make down
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
             # XXX make reachc clean
             #clean)
             #    ../../reach clean
             #    ;;
         esac
     fi) || exit 1
done
