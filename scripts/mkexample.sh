#!/bin/sh -e
NAME="$1"
! [ "x${NAME}" = "x" ]

DIR="examples/${NAME}"
! [ -d "${DIR}" ]

mkdir "${DIR}"
cd "${DIR}"
../../reach init
touch index.txt
echo "build/" > .gitignore
git add index.* .gitignore
