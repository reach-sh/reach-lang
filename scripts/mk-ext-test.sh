#!/bin/sh
# Creates a directory in `examples` to test Reach code external to this project.
#
# Usage:
#   ./mk-ext-test.sh <dir-name> <git-url>

NAME="$1"
REPO="$2"

cd ../examples  || exit 1
mkdir "$NAME"   || exit 2
cd "$NAME"      || exit 3

touch Makefile

cat >>Makefile <<END
all: build

REACH = ../../reach

.PHONY: clean
clean:
	rm -f index.rsh index.mjs
	rm -rf ${NAME}

checkout:
	if test -d ${NAME}; then echo "Repo already cloned"; else git clone ${REPO} ${NAME}; fi

.PHONY: build
build: checkout
	cd ${NAME} && make build

.PHONY: run
run:
	echo "external code will not be ran"
END
