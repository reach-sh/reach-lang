#!/bin/sh

go () {
  ORIG="$1"
  if ! [ -f "$ORIG" ] ; then
    echo "$ORIG" does not exist
    exit 1
  fi
  FORK="forks/$(basename "$1")"
  PATCH="${FORK}.patch"
  if ! [ -f "$PATCH" ] ; then
    diff -u "${ORIG}" "${FORK}" > "${PATCH}"
    echo "${ORIG}" - patch created
  else
    if cmp "${ORIG}" "${FORK}" > /dev/null ; then
      echo "${ORIG}" - matches
    else
      patch -u "${ORIG}" "${PATCH}"
    fi
  fi
}

go "node_modules/@vuepress/markdown/lib/highlight.js"
