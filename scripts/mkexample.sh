#!/bin/sh -e
NAME="$1"
[ "${NAME}" = "" ] && exit 1

DIR="examples/${NAME}"
[ -d "${DIR}" ] && exit 1

mkdir "${DIR}"
cd "${DIR}"
../../reach init
touch index.txt
echo "build/" > .gitignore
git add index.* .gitignore
