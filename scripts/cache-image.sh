#!/bin/sh -x
IMAGE=$1

aws ecr create-repository --repository-name "${IMAGE}" || true

for i in $(docker image ls --format "{{.Repository}}:{{.Tag}}" | grep "${IMAGE}:circleci") ; do
  docker push "$i"
done
