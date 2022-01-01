#!/bin/sh
TOTALP1=$(find ../examples -maxdepth 1 -type d | wc -l)
TOTAL=$((TOTALP1 - 1))

PRE=config.pre.yml
MID=config.mid.yml
END=config.end.yml
cat >"${MID}" </dev/null
cat >"${END}" </dev/null

cat >>"${END}" <<END
    - "examples-sink":
        requires:
END

for CONN in ETH ALGO CFX ; do
  CONNlc=$(echo "${CONN}" | tr '[:upper:]' '[:lower:]')
  IMAGE="devnet-${CONNlc}"
  case "${CONN}" in
    ALGO) PER=8  ;;
     CFX) PER=8  ;;
     ETH) PER=16 ;;
  esac
  SIZE=$(((TOTAL + (PER - 1)) / PER))
  NAME="examples.${CONN}"
  cat >>"${MID}" <<END
    - "examples":
        name: "${NAME}"
        connector: "${CONN}"
        size: ${SIZE}
        deps: "${IMAGE}"
        requires:
          - "build"
END
  cat >>"${END}" <<END
          - "${NAME}"
END
done

cat "${PRE}" "${MID}" "${END}" > config.gen.yml
