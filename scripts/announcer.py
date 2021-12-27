import requests
import os

webhook_token = os.environ['DISCORD_WEBHOOK_TOKEN']
webhook_id = os.environ['DISCORD_WEBHOOK_ID']
version = os.environ['VERSION']
rc_version = os.environ['RC_VERSION']
git_commit_hash = os.environ['GIT_COMMIT_HASH']

url = "https://discord.com/api/webhooks/{}/{}".format(webhook_id, webhook_token)
message = open('message.txt').read().format(version, git_commit_hash, rc_version)
payload = {"content": message, "username": "Announcer"}
request = requests.post(url, data=payload)

print("Announcement to Discord returned: {}".format(request.status_code))
