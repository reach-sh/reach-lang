#! /bin/sh

HERE=.
echo "latest
${MAJOR}
${MAJOR}.${MINOR}
${VERSION}
${RC_VERSION}" > "${HERE}/versions.txt"

