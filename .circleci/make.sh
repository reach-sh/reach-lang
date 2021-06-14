#!/bin/sh

DEST=config.gen.yml

cat config.pre.yml > "${DEST}"
for ep in ../examples/* ; do
  if [ -d "${ep}" ] ; then
    e=$(basename "${ep}")
    cat >>"${DEST}" <<END
    - "example":
        name: "examples.${e}"
        which: "${e}"
        requires:
          - "build"
END
  fi
done

cat >>"${DEST}" <<END
    - "example-sink":
        requires:
END
for ep in ../examples/* ; do
  if [ -d "${ep}" ] ; then
    e=$(basename "${ep}")
    cat >>"${DEST}" <<END
          - "examples.${e}"
END
  fi
done
