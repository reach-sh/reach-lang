#!/bin/sh
IMAGE=$1
images=$(docker image ls | grep "${IMAGE}:circleci" | awk '{print $1 ":" $2}')
for i in ${images}; do
  docker push "$i"
done
