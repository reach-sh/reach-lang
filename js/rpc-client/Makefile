.DEFAULT_GOAL := build

ROOT=../..
include $(ROOT)/js/JS_IMAGES

IMAGE=$(RPC_CLIENT_IMAGE)

.PHONY: install-mo
install-mo:
	curl -sSL https://git.io/get-mo -o mo
	chmod +x mo
	sudo mv mo /usr/local/bin

.PHONY: mo
mo:
	@which mo 1>/dev/null || (echo 'FAILURE: Please call `make install-mo` first' && exit 1)

package.json: package.mo.json $(ROOT)/VERSION mo
	REACH_VERSION=$(REACH_VERSION) mo $< > $@

.PHONY: build
build: package.json
	docker build --tag=$(IMAGE):latest \
	  --build-arg RUNNER_VERSION=$(RUNNER_IMAGE):$(REACH_VERSION) \
	  .
	TAG_ONLY=1 $(ROOT)/scripts/docker-push.sh $(IMAGE)

.PHONY: clean
clean:
	rm -f package.json
