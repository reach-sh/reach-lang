ROOT=../..
include $(ROOT)/js/JS_IMAGES

IMAGE=$(JS_DEPS_IMAGE)

.PHONY: update-lock
update-lock:
	REFRESH_LOCK=Y $(MAKE) build
	docker run --rm --entrypoint cat $(IMAGE) /js-deps/package-lock.json > package-lock.json

.PHONY: build
build:
	docker build --tag=$(IMAGE):latest \
	  --build-arg NODE_VERSION=$(NODE_VERSION) \
	  --build-arg REACH_GIT_HASH=$(REACH_GIT_HASH) \
	  --build-arg REFRESH_LOCK=${REFRESH_LOCK} \
	  .
	TAG_ONLY=1 $(ROOT)/scripts/docker-push.sh $(IMAGE)
