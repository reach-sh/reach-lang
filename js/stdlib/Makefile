ROOT=../..
include $(ROOT)/VERSION
include $(ROOT)/js/JS_IMAGES
include $(ROOT)/DEPS
include Makefile.docker

IMAGE=$(STDLIB_IMAGE)
JS-PACKAGE='@reach-sh/stdlib'

$(ROOT)/hs/sol/stdlib.json:
	cd $(ROOT)/hs && $(MAKE) sol/stdlib.json

stdlib_sol.json: $(ROOT)/hs/sol/stdlib.json
	cp -f $< $@

.PHONY: expand
expand: package.json ts/version.ts ts/stdlib_sol.ts

.PHONY: build
build-stdlib-build:
	mkdir -p .docker-root
	cp -f $(ROOT)/VERSION .docker-root/
	docker build --tag=$(IMAGE):build-latest \
	  --build-arg NODE_VERSION=$(NODE_VERSION) \
	  --build-arg REACH_VERSION=$(VERSION) \
	  --build-arg REACH_GIT_HASH=$(REACH_GIT_HASH) \
	  --cache-from=$(IMAGE):build-${CIRCLE_BRANCH} \
	  --cache-from=$(IMAGE):build-master \
	  .

.PHONY: build
build-stdlib:
	mkdir -p .docker-root
	cp -f $(ROOT)/VERSION .docker-root/
	docker build --tag=$(IMAGE) \
	  --build-arg NODE_VERSION=$(NODE_VERSION) \
	  --build-arg REACH_VERSION=$(VERSION) \
	  --build-arg REACH_GIT_HASH=$(REACH_GIT_HASH) \
	  --cache-from=$(IMAGE):build-${CIRCLE_BRANCH} \
	  --cache-from=$(IMAGE):build-master \
	  --cache-from=$(IMAGE):latest \
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

.PHONY: clone-js-stdlib
clone-js-stdlib:
	rm -rf js-reach-stdlib
	git clone -b CORE-171 https://github.com/reach-sh/js-reach-stdlib.git
	cp -r sbin js-reach-stdlib/sbin

.PHONY: refresh-js-stdlib
refresh-js-stdlib: clone-js-stdlib
	cd js-reach-stdlib && ./sbin/refresh.sh

.PHONY: bump-version-and-publish-js-stdlib
bump-version-and-publish-js-stdlib: 
	cd js-reach-stdlib ; \
	echo "//registry.npmjs.org/:_authToken=${NPM_TOKEN}" > .npmrc; \
	npm publish --access=public --tag=rc; \
	git tag $(VERSION); \
	git push; \
	git push --tags

.PHONY: finalize-patch-js-stdlib
finalize-patch-js-stdlib:
	cd js-reach-stdlib
	echo "//registry.npmjs.org/:_authToken=${NPM_TOKEN}" > .npmrc
	npm version patch
	npm publish --access=public --tag=stable
	npm dist-tag add $(JS-PACKAGE)"@$$(npm view $(JS-PACKAGE)@stable version)" latest
	git push
	git push --tags