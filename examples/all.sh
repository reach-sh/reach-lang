#!/bin/sh

MODE="$1"

for e in $(find . -depth 1 -type d) ; do
    echo "$e"
    cd "$e"
    if [ -f Makefile ] ; then
        make "$MODE"
        RESULT=$?
        if [ "$MODE" = "build" ] && [ "$RESULT" -ne 0 ] ; then
            echo "$e" build failed
            exit 1
        fi
        if [ "$MODE" = "run" ] ; then
            make down
        fi
    else
        case "$MODE" in
            build)
                ../../reach compile
                ;;
            run)
                ../../reach run
                ;;
        esac
    fi
    cd ..
done
