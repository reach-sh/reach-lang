#!/bin/bash

export FINAL_VERSION=$(echo $VERSION | awk -F '-' '{print $1}')

url="https://discord.com/api/webhooks/${DISCORD_WEBHOOK_ID}/${DISCORD_WEBHOOK_TOKEN}"
mo message.txt.mo


message=$(cat message.txt)
payload="{\"content\": \"${message}\", \"username\": \"Announcer\"}"

curl -X POST -H "Content-Type: application/json" -d "${payload}" "${url}"