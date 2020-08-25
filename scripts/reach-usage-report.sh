#!/bin/bash

# Requires curl, awscli, and jq
# usage: ./reach-usage-report.sh | jq

# https://docs.docker.com/registry/spec/api/

echo '{'

echo '  "repository_stats":'
curl -s https://hub.docker.com/v2/repositories/reachsh/reach/ | \
  jq '{pull_count: .pull_count, last_updated: .last_updated}'

echo ', "tag_info":'
curl -s https://hub.docker.com/v2/repositories/reachsh/reach/tags/ | \
  jq '.results | map({name: .name, who: .last_updater_username, when: .last_updated})'


# https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Query.html

# TODO: is paging an issue when the table gets larger?
echo ', "CompileLog_count":'
aws dynamodb scan \
    --table-name CompileLog \
    --consistent-read \
    --select COUNT | \
  jq '.Count'

# TODO: distinct user count

echo '}'
