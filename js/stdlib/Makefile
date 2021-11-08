ROOT=../..
include $(ROOT)/js/JS_IMAGES
include $(ROOT)/DEPS
include Makefile.docker

IMAGE=$(STDLIB_IMAGE)

$(ROOT)/hs/sol/stdlib.json:
	cd $(ROOT)/hs && $(MAKE) sol/stdlib.json

stdlib_sol.json: $(ROOT)/hs/sol/stdlib.json
	cp -f $< $@

.PHONY: expand
expand: package.json ts/version.ts ts/stdlib_sol.ts

.PHONY: build
build:
	mkdir -p .docker-root
	cp -f $(ROOT)/VERSION .docker-root/
	docker build --tag=$(IMAGE) \
	  --build-arg NODE_VERSION=$(NODE_VERSION) \
	  --build-arg REACH_VERSION=$(VERSION) \
	  --build-arg REACH_GIT_HASH=$(REACH_GIT_HASH) \
	  .
	TAG_ONLY=1 $(ROOT)/scripts/docker-push.sh $(IMAGE)

.PHONY: push
push:
	$(ROOT)/scripts/docker-push.sh $(IMAGE)

.PHONY: format
format: expand
	npm run beautify
	npm run format

.PHONY: check
check: expand
	sbin/check.sh

.PHONY: js-build
js-build: expand
	npm run build

# XXX should be preceded by js-build
# but don't want to re-force it
.PHONY: js-mjs
js-mjs: expand
	sbin/fix.sh
