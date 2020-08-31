#!/bin/sh

ANALYTICS=$(cat analytics.html)

find ../docs -name '*.html' |
    while read -r HTML ; do
        sed -i '' -e "s#<head>#<head>${ANALYTICS}#" "$HTML"
    done
