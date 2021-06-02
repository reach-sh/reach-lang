ROOT=../..
include $(ROOT)/js/JS_IMAGES

IMAGE=$(STDLIB_IMAGE)

.PHONY: clean
clean:
	rm -f package.json

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

ts/version.ts: ts/version.mo.ts $(ROOT)/VERSION mo
	REACH_VERSION=$(REACH_VERSION) mo $< > $@

.PHONY: build
build: package.json ts/version.ts
	docker build --tag=$(IMAGE) \
	  --build-arg NODE_VERSION=$(NODE_VERSION) \
	  --build-arg REACH_VERSION=$(VERSION) \
	  --build-arg REACH_GIT_HASH=$(REACH_GIT_HASH) \
	  .
	TAG_ONLY=1 $(ROOT)/scripts/docker-push.sh $(IMAGE)

.PHONY: format
format: package.json
	npm run beautify
	npm run format

.PHONY: check
check: package.json
	sbin/check.sh

.PHONY: js-build
js-build: package.json
	npm run build

# XXX should be preceded by js-build
# but don't want to re-force it
.PHONY: js-mjs
js-mjs: package.json
	sbin/fix.sh
