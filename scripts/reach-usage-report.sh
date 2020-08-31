#!/bin/bash

# Requires curl, awscli, and jq
# usage: ./reach-usage-report.sh | jq

# https://docs.docker.com/registry/spec/api/

PROJ_ROOT="$(dirname "$(realpath "${0}")")/.."

# shellcheck source=VERSION
. "${PROJ_ROOT}/VERSION"

echo '{'

echo '  "report_date":'
echo "\"$(TZ=Z date +%FT%TZ)\""

echo ', "repository_stats":'
curl -s https://hub.docker.com/v2/repositories/reachsh/reach/ | \
  jq '{pull_count: .pull_count, last_updated: .last_updated}'

echo ', "tag_info": {'
echo '    "json": "is hard"'
for CONTAINER in reach stdlib runner ethereum-devnet ; do

  echo ",   \"$CONTAINER\":"
  curl -s "https://hub.docker.com/v2/repositories/reachsh/$CONTAINER/tags/" | \
    jq ".results | map(select(.name == \"$VERSION\") | {name: .name, who: .last_updater_username, when: .last_updated})"

done
echo '}'

# https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Query.html

# TODO: is paging an issue when the table gets larger?
echo ', "CompileLog":'
CompileLog=$(
aws dynamodb scan \
    --table-name CompileLog \
    --consistent-read \
    --select SPECIFIC_ATTRIBUTES \
    --projection-expression userId
)
echo "$CompileLog" | jq '{row_count: .Count, unique_users: (.Items | map(.userId.S) | unique | length)}'

echo '}'
