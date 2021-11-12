import requests
import os

def announcer(webhook_token, webhook_id, release_version, git_commit_hash):
    url = "https://discord.com/api/webhooks/{}/{}".format(webhook_id, webhook_token)
    if "rc" in release_version:
        final_release_version = release_version.split("-")[0]
    else:
        final_release_version = release_version
    message = open('message.txt').read().format(final_release_version, git_commit_hash, release_version)
    payload = {"content": message, "username": "Announcer"}
    request = requests.post(url, data=payload)
    return request

discord_webhook_token = os.environ['DISCORD_WEBHOOK_TOKEN']
discord_webhook_id = os.environ['DISCORD_WEBHOOK_ID']
version = os.environ['VERSION']
git_commit_hash = os.environ['GIT_COMMIT_HASH']

request = announcer(discord_webhook_token, discord_webhook_id, version, git_commit_hash)

print("Announcement to discord returned: {}".format(request.status_code))