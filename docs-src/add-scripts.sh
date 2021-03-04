#!/bin/sh

PREF="scripts-pre.html"
POSTF="scripts-post.html"
SEARCHF="scripts-search.html"

check() {
    FILE="$1"
    LINES=$(wc -l "${FILE}" | awk '{print $1}')
    if [ "$LINES" -ne 1 ] ; then
        echo "${FILE}" must be one line
        exit 1
    fi
}

check "${PREF}"
check "${POSTF}"
check "${SEARCHF}"

PRE=$(cat "${PREF}")
POST=$(cat "${POSTF}")
SEARCH=$(cat "${SEARCHF}")

find ../docs -name '*.html' |
    while read -r HTML ; do
        sed -i.bak \
          -e "s#<head>#<head>${PRE}#" \
          -e "s#</body>#${POST}</body>#" \
          -e "s#<div class=\"nosearchform\"></div>#${SEARCH}#" \
          "$HTML"
        rm "$HTML".bak
    done
