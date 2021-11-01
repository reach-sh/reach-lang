.DEFAULT_GOAL := build

ROOT=../..
include $(ROOT)/js/JS_IMAGES
include $(ROOT)/DEPS
include Makefile.docker

IMAGE=$(RPC_SERVER_IMAGE)

.PHONY: expand
expand: package.json

.PHONY: build
build:
	mkdir -p .docker-root
	cp -f $(ROOT)/VERSION .docker-root/
	docker build --tag=$(IMAGE):latest \
	  --build-arg RUNNER_VERSION=$(RUNNER_IMAGE):$(REACH_VERSION) \
	  --build-arg REACH_VERSION=$(VERSION) \
	  .
	TAG_ONLY=1 $(ROOT)/scripts/docker-push.sh $(IMAGE)

.PHONY: clean
clean:
	rm -f package.json
