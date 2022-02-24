.DEFAULT_GOAL := build
IMAGE=reachsh/rpc-server
ROOT=../..
include Makefile.docker

.PHONY: expand
expand: package.json

.PHONY: build
build:
	$(ROOT)/scripts/build-image.sh $(IMAGE) Dockerfile
	TAG_ONLY=1 $(ROOT)/scripts/docker-push.sh $(IMAGE)

.PHONY: clean
clean:
	rm -f package.json
