ROOT=../..
include $(ROOT)/js/JS_IMAGES

IMAGE=$(RUNNER_IMAGE)

.PHONY: build
build:
	docker build --tag=$(IMAGE):latest \
	  --build-arg REACH_VERSION=$(REACH_VERSION) \
	  --build-arg REACH_GIT_HASH=$(REACH_GIT_HASH) \
	  .
	TAG_ONLY=1 $(ROOT)/scripts/docker-push.sh $(IMAGE)
