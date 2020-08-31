#!/bin/sh

ANALYTICS=$(cat analytics.html)

find ../docs -name '*.html' |
    while read -r HTML ; do
        # Portable on mac & linux which have different -i behavior
        # https://stackoverflow.com/a/22084103/208257
        sed -i.bak -e "s#<head>#<head>${ANALYTICS}#" "$HTML"
        rm "$HTML".bak
    done
