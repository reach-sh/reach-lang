#!/bin/bash
$IMAGE_NAME=$1
$TAG=$2

function get_token() {
    ENDPOINT="https://hub.docker.com/v2/users/login"
    curl -H 'Content-Type: application/json' -X POST \
      -d "{\"username\": \"${DOCKER_USERNAME}\", \"password\": \"${DOCKER_PASSWORD}\"}" \
      https://hub.docker.com/v2/users/login | jq .token | sed -e 's/\"//g'
}

function get_digest() {
    IMAGE_NAME=$1
    TAG=$2
    TOKEN=$3
    ENDPOINT="https://hub.docker.com/v2/namespaces/reachsh/repositories/${IMAGE_NAME}/images?ordering=-last_activity&currently_tagged=true&page_size=1000"

    TAGS_NOTFORMATED=$(curl -H "Authorization: Bearer ${TOKEN}" $ENDPOINT | \
    jq ".results[] | select(.tags[] | (.tag == \"${TAG}\") and (.is_current == true)) | (.tags[].tag)")

    DIGEST=$(curl -H "Authorization: Bearer ${TOKEN}" $ENDPOINT | \
    jq ".results[] | select(.tags[] | (.tag == \"${TAG}\") and (.is_current == true)) | (.digest)")

    TAGS=$(echo $TAGS_NOTFORMATED | sed -e "s/\ /,/g")
}

function delete_image() {
    IMAGE_NAME=$1
    TAG=$2
    TOKEN=$3
    get_digest $IMAGE_NAME $TAG $TOKEN
    PAYLOAD="{\"manifests\": [{\"repository\": \"${IMAGE_NAME}\", \"digest\": ${DIGEST}}], \"ignore_warnings\": [{\"repository\": \"${IMAGE_NAME}\", \"digest\": ${DIGEST}, \"warning\": \"is_active\"},{\"repository\": \"${IMAGE_NAME}\", \"digest\": ${DIGEST}, \"warning\": \"current_tag\", \"tags\": [${TAGS}]}]}"

    curl -H "Authorization: Bearer ${TOKEN}" \
        -H "Content-type: application/json" \
        'https://hub.docker.com/v2/namespaces/reachsh/delete-images' \
        -d "${PAYLOAD}"
}

TOKEN=$(get_token)
delete_image $IMAGE_NAME $TAG $TOKEN