#!/bin/bash

# Requires curl, awscli, and jq
# usage: ./reach-usage-report.sh | jq

HERE=$(dirname "$(realpath "$0")")
REPORT_FILE='report.csv'

# https://docs.docker.com/registry/spec/api/

UsageReport=$("$HERE/reach-usage-report.sh")

echo 'report_date,pull_count,row_count,unique_users' > $REPORT_FILE
echo $UsageReport | jq '[.report_date, .repository_stats.pull_count, .CompileLog.row_count, .CompileLog.unique_users] | @csv' -r >> $REPORT_FILE
