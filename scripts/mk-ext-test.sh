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

.PHONY: clean
clean:
	rm -rf ../ext-${NAME}

checkout:
	if test -d ../ext-${NAME}; then echo "Repo already cloned"; else git clone ${REPO} ../ext-${NAME}; fi

.PHONY: build
build: checkout
	cd ../ext-${NAME} && ../one.sh build

# TODO: Check that it makes sense to run the code. If not, echo instead.
.PHONY: run
run:
	cd ../ext-${NAME} && ../one.sh run
# echo "External code will not be run."
END
