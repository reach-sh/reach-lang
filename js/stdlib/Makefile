ROOT=../..
include $(ROOT)/js/JS_IMAGES

IMAGE=$(STDLIB_IMAGE)
REACH="$(shell cd ../.. && pwd)/reach"

.PHONY: clean
clean: clean-test
	rm -f package.json

.PHONY: clean-test
clean-test:
	[ -d test/stdlib-test ] && rm -r test/stdlib-test || true

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
	docker build --tag=$(IMAGE) \
	  --build-arg NODE_VERSION=$(NODE_VERSION) \
	  --build-arg REACH_VERSION=$(VERSION) \
	  --build-arg REACH_GIT_HASH=$(REACH_GIT_HASH) \
	  .
	TAG_ONLY=1 $(ROOT)/scripts/docker-push.sh $(IMAGE)

# Note: We're creating an `stdlib-test` directory from scratch here in order
#   to leverage the `reach` script and abstract over boilerplate such as
#   `docker-compose.yml` config.
.PHONY: test
test: build clean-test
	mkdir test/stdlib-test
	cp    test/index.* test/stdlib-test
	cp -r test/lib     test/stdlib-test/lib
	cd test/stdlib-test \
	  && $(REACH) compile \
	  && REACH_CONNECTOR_MODE=ALGO $(REACH) run \
	  && REACH_CONNECTOR_MODE=ETH  $(REACH) run \
	  && REACH_CONNECTOR_MODE=FAKE $(REACH) run \
	   ; $(REACH) down

.PHONY: format
format: package.json
	npm run beautify
	npm run format
