#!/bin/bash

# Requires curl, awscli, and jq
# usage: ./reach-usage-report.sh | jq

# https://docs.docker.com/registry/spec/api/

HERE="$(dirname "$(realpath "${0}")")";
UP_TO="$1"

# shellcheck source=../VERSION
. "${HERE}/../VERSION"

echo '{'

if [ "x$UP_TO" = "x" ] ; then
  DATE="$(TZ=Z date +%FT%TZ)"
else
  DATE="$UP_TO"
fi
echo '  "report_date":'
echo "\"$DATE\""

echo ', "repository_stats":'
curl -s https://hub.docker.com/v2/repositories/reachsh/reach/ | \
  jq '{pull_count: .pull_count, last_updated: .last_updated}'

echo ', "tag_info": {'
echo '    "json": "is hard"'
for CONTAINER in reach stdlib runner ethereum-devnet algorand-devnet ; do

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
    --projection-expression userId,startTime
)

if [ "x$UP_TO" = "x" ] ; then
  echo "$CompileLog" | jq '{row_count: .Count, unique_users: (.Items | map(.userId.S) | unique | length)}'
else
  UP_TO_T="${UP_TO}T23:59:59.999999999Z"
  echo "$CompileLog" | jq '{row_count: .Count, unique_users: (.Items | map(select(.startTime.S <= "'"$UP_TO_T"'")) | map(.userId.S) | unique | length)}'
fi

echo '}'
