#! /bin/sh
# Usage: sbin/check.sh
# (call from js/stdlib/)

# This is a very fragile and slightly incorrect way of checking
# that this package.json hasn't drifted too far away
# from js-deps.

# Basically, when js-deps updates its dependencies,
# they need to get updated here, too.

N_DIFF_LINES="$(diff ../js-deps/package.json ./package.json | wc -l)"
if [ "$N_DIFF_LINES" -eq 37 ]; then
	echo 'The diff line count looks good'
else
	echo 'The diff line count looks off'
	set -ex
	diff ../js-deps/package.json ./package.json | wc -l
	diff ../js-deps/package.json ./package.json
fi
