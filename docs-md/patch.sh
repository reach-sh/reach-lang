#!/bin/sh

go () {
  ORIG="$1"
  if ! [ -f "$ORIG" ] ; then
    echo "$ORIG" does not exist
    exit 1
  fi
  FORK="patches/$(basename "$1")"
  PATCH="${FORK}.patch"
  if ! [ -f "$PATCH" ] ; then
    diff -u "${ORIG}" "${FORK}" > "${PATCH}"
    echo "${ORIG}" - patch created
  fi
  if cmp "${ORIG}" "${FORK}" > /dev/null ; then
    echo "${ORIG}" - matches
  else
    patch -u "${ORIG}" "${PATCH}"
    echo "${ORIG}" - patched
  fi
}

# cp forks/reach.tmLanguage.json node_modules/shiki/languages/
