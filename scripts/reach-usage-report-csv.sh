#!/bin/bash

# Requires curl, awscli, and jq
# usage: ./reach-usage-report-csv.sh

# If you specify the optional argument,
# it MUST be in the form YYYY-MM-DD

HERE=$(dirname "$(realpath "$0")")
REPORT_FILE='report.csv'
UP_TO="$1"

# https://docs.docker.com/registry/spec/api/

UsageReport=$("$HERE/reach-usage-report.sh" "$UP_TO")

echo 'report_date,pull_count,row_count,unique_users' > $REPORT_FILE
if [ "x$UP_TO" = "x" ] ; then
  echo "$UsageReport" | jq -r '[.report_date, .repository_stats.pull_count, .CompileLog.row_count, .CompileLog.unique_users] | @csv' >> $REPORT_FILE
else
  echo "$UsageReport" | jq -r '["'"${UP_TO}"'", "n/a", "n/a", .CompileLog.unique_users] | @csv' > $REPORT_FILE
fi
echo "Wrote report to $REPORT_FILE"
