IMAGE=${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/js-deps
ROOT=../..

.PHONY: update-lock
update-lock:
	REFRESH_LOCK=Y $(MAKE) build
	docker run --rm --entrypoint cat $(IMAGE) /js-deps/package-lock.json > package-lock.json

.PHONY: build
build:
	$(ROOT)/scripts/build-image.sh $(IMAGE) Dockerfile \
	  --build-arg REFRESH_LOCK=${REFRESH_LOCK}
#	TAG_ONLY=1 $(ROOT)/scripts/docker-push.sh $(IMAGE)
