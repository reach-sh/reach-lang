ROOT=../..
include $(ROOT)/js/JS_IMAGES

IMAGE=$(RUNNER_IMAGE)

.PHONY: install-mo
install-mo:
	curl -sSL https://git.io/get-mo -o mo
	chmod +x mo
	sudo mv mo /usr/local/bin

.PHONY: mo
mo:
	@which mo 1>/dev/null || (echo 'FAILURE: Please call `make install-mo` first' && exit 1)

package.json: package.mo.json $(ROOT)/VERSION mo
	VERSION=$(VERSION) mo $< > $@

.PHONY: build
build: package.json
	docker build --tag=$(IMAGE):latest \
 	  --build-arg NODE_VERSION=$(NODE_VERSION) \
 	  --build-arg REACH_VERSION=$(REACH_VERSION) \
 	  --build-arg REACH_GIT_HASH=$(REACH_GIT_HASH) \
	  .
	TAG_ONLY=1 $(ROOT)/scripts/docker-push.sh $(IMAGE)

# TODO: replace build w/ build2,
# in the style of react_runner
.PHONY: build2
build2:
	echo TODO build2-runner
