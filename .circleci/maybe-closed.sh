#!/bin/sh -xe
REPO=reach-everest
REMOTE="git@github.com:reach-sh/${REPO}.git"

JOB="$1"
if ! (grep closed "$JOB" > /dev/null) ; then
  echo "Not closed"
  exit 0
fi

go () {
  BRANCH="$1"
  echo "Making shallow clone of '$BRANCH'"
  git clone --depth 1 --branch "$BRANCH" --single-branch "$REMOTE" "$REPO"
}

if (go "$CIRCLE_BRANCH") ; then
  echo Closed has same branch
else
  go "master"
fi

RPZ=$(realpath "$0")
HERE="$(dirname "$RPZ")"
exec "${HERE}/link.sh"
