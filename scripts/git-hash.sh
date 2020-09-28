#!/bin/sh

GITHASH="$(git rev-parse --short HEAD)"

if [ -n "$(git status -s 2> /dev/null)" ] ; then
  GITDIRTY="*"
else
  GITDIRTY=""
fi

echo "$GITHASH$GITDIRTY"
