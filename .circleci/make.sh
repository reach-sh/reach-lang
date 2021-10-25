#!/bin/sh
TOTALP1=$(find ../examples -maxdepth 1 -type d | wc -l)
TOTAL=$((TOTALP1 - 1))

PRE=config.pre.yml
MID=config.mid.yml
END=config.end.yml

cat >>"${END}" <<END
    - "examples-sink":
        requires:
END

for CONN in ETH ALGO CFX ; do
  case "${CONN}" in
    ALGO) 
      PER=8
      DEP="build-devnet-algo"
      IMAGES="runner rpc-server reach reach-cli devnet-algo"
      ;;
    CFX)
      PER=8
      DEP="build-devnet-cfx"
      IMAGES="runner rpc-server reach reach-cli devnet-algo devnet-eth"
      ;;
    ETH)
      PER=16
      DEP="build-devnet-eth"
      IMAGES="runner rpc-server reach reach-cli devnet-algo devnet-cfx"
      ;;
  esac
  SIZE=$(((TOTAL + (PER - 1)) / PER))
  for RANK in $(seq 0 $((SIZE - 1))) ; do
    cat >>"${MID}" <<END
    - "examples":
        name: "examples.${CONN}.${RANK}"
        filters:
          tags:
            only: /[0-9]*\.[0-9]*\.[0-9]*/
        connector: "${CONN}"
        size: "${SIZE}"
        rank: "${RANK}"
        images: "${IMAGES}"
        requires:
          - "build-devnet-algo"
          - "build-runner"
          - "build-rpc-server"
          - "build-reach"
          - "build-reach-cli"
          - "${DEP}"
END
    cat >>"${END}" <<END
          - "examples.${CONN}.${RANK}"
END
  done
done

cat "${PRE}" "${MID}" "${END}" > config.gen.yml
