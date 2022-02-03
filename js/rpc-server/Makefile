.DEFAULT_GOAL := build
IMAGE=${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/rpc-server
ROOT=../..
include Makefile.docker

.PHONY: expand
expand: package.json

.PHONY: build
build:
	mkdir -p .docker-root
	cp -f $(ROOT)/VERSION .docker-root/
	$(ROOT)/scripts/build-image.sh $(IMAGE) Dockerfile
#	TAG_ONLY=1 $(ROOT)/scripts/docker-push.sh $(IMAGE)

.PHONY: clean
clean:
	rm -f package.json
