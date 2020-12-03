#!/bin/bash

# Requires curl, awscli, and jq
# usage: ./reach-usage-report.sh | jq

# https://docs.docker.com/registry/spec/api/

HERE="$(dirname "$(realpath "${0}")")";

# shellcheck source=../VERSION
. "${HERE}/../VERSION"

echo '{'

DATE="$(TZ=Z date +%FT%TZ)"

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

formatMonth () {
  if [ "$1" -lt 10 ]; then
    echo 0"$1"
  else
    echo "$1"
  fi
}

uniqueUserBuilder=""
declare -a years=(2020)
for year in "${years[@]}"; do
  for ((i=9; i<=12; i++)); do
    formatI=$(formatMonth "$i")
    uniqueUserBuilder+="\"$year-$i\": (.Items | map(select(.startTime.S | startswith(\"$year-$formatI\") ))), ";
  done
done
# Remove last ', ' from string
uniqueUsers="${uniqueUserBuilder%)*})"

CompileLog=$(
  aws dynamodb scan \
    --table-name CompileLog \
    --select SPECIFIC_ATTRIBUTES \
    --projection-expression userId,startTime
)
echo "$CompileLog" | jq '{ row_count: .Count, unique_users: { '"$uniqueUsers"' }}'

echo '}'
