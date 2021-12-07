#!/bin/bash
IMAGE_NAME=$1

images=$(docker image ls | grep $IMAGE_NAME | grep $CIRCLE_BRANCH | awk '{print $1 ":" $2}') 

for i in $images; do
  docker push $i
done
