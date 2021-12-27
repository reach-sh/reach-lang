#!/bin/sh
TOTALP1=$(find ../examples -maxdepth 1 -type d | wc -l)
TOTAL=$((TOTALP1 - 1))

PRE=config.pre.yml
MID=config.mid.yml
END=config.end.yml
cat >"${MID}" </dev/null
cat >"${END}" </dev/null

deps () {
  DEPS="$*"
  cat >>"${MID}" <<END
        deps: "$DEPS"
END
  cat >>"${MID}" <<END
        requires:
          - "build"
END
}

cat >>"${END}" <<END
    - "examples-sink":
        requires:
END

for CONN in ETH ALGO CFX ; do
  CONNlc=$(echo "${CONN}" | tr '[:upper:]' '[:lower:]')
  IMAGE="devnet-${CONNlc}"
  case "${CONN}" in
    ALGO) PER=8 ;;
    CFX) PER=8 ;;
    ETH) PER=16 ;;
  esac
  SIZE=$(((TOTAL + (PER - 1)) / PER))
  for RANK in $(seq 0 $((SIZE - 1))) ; do
    NAME="examples.${CONN}.${RANK}"
    cat >>"${MID}" <<END
    - "examples":
        name: "${NAME}"
        connector: "${CONN}"
        size: "${SIZE}"
        rank: "${RANK}"
END
    deps "${IMAGE}"
    cat >>"${END}" <<END
          - "${NAME}"
END
  done
done

cat "${PRE}" "${MID}" "${END}" > config.gen.yml
