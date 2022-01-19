#!/bin/bash -e
HERE="$(dirname "$0")"
cd "$HERE"

WHICH="$1"
FROM="$2"
TO="$3"

HOST="reach-algo-${WHICH}"
REACH_ONLY="txn->'txn'->>'note' LIKE 'UmVhY2gg%'"
IS_CREATION="txn->'txn' ? 'apap'"

PSQL=(psql --host=127.0.0.1 -d ledgerdb -U ledgerdb -qtA)

cat >go.sh <<END
#!/bin/sh -e
getRound () {
  ${PSQL[@]} -c "SELECT round FROM block_header WHERE realtime >= '\$1' ORDER BY realtime ASC LIMIT 1;"
}

FROM_ROUND=\$(getRound '$FROM')
TO_ROUND=\$(getRound '$TO')

HEAD="SELECT COUNT(*) FROM (SELECT txn FROM txn WHERE txn->'txn'->>'type' = 'appl' AND round < \${TO_ROUND} AND round >= \${FROM_ROUND}"
TAIL=") t;"

ALL_TXNS=\$(${PSQL[@]} -c "\${HEAD} \${TAIL}")
RSH_TXNS=\$(${PSQL[@]} -c "\${HEAD} AND ${REACH_ONLY} \${TAIL}")

ALL_CTOR=\$(${PSQL[@]} -c "\${HEAD} AND ${IS_CREATION} \${TAIL}")
RSH_CTOR=\$(${PSQL[@]} -c "\${HEAD} AND ${IS_CREATION} AND ${REACH_ONLY} \${TAIL}")

echo "${FROM},\${FROM_ROUND},${TO},\${TO_ROUND},\${RSH_TXNS},\${ALL_TXNS},\${RSH_CTOR},\${ALL_CTOR}"
END
chmod +x go.sh
scp -q go.sh "${HOST}:"
ssh "${HOST}" ./go.sh
