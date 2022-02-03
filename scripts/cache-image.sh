#!/bin/sh -x
IMAGE=$1

for i in $(docker image ls --format "{{.Repository}}:{{.Tag}}" | grep "${IMAGE}:circleci") ; do
  docker push "${i}"
done
