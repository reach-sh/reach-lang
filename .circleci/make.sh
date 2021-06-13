#!/bin/sh

DEST=config.gen.yml

cat config.pre.yml > "${DEST}"
for ep in ../examples/* ; do
  if [ -d "${ep}" ] ; then
    e=$(basename "${ep}")
    cat >>"${DEST}" <<END
    - example:
        name: "examples.${e}"
        which: "${e}"
        requires:
          - "build"
END
  fi
done

cat >>"${DEST}" <<END
    - sink:
        context:
          - "circleci-on-slack"
        requires:
          - "docs-render"
          - "shellcheck"
          - "build"
          - "hs-test"
          - "hs-check"
          - "js-test"
END
for ep in ../examples/* ; do
  if [ -d "${ep}" ] ; then
    e=$(basename "${ep}")
    cat >>"${DEST}" <<END
          - "examples.${e}"
END
  fi
done
