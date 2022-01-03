IMAGE=reachsh/react-runner
ROOT=../..

.PHONY: build
build:
	$(ROOT)/scripts/build-image.sh $(IMAGE) Dockerfile
	TAG_ONLY=1 $(ROOT)/scripts/docker-push.sh $(IMAGE)
