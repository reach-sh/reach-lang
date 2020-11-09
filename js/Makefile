ROOT=..
include $(ROOT)/js/JS_IMAGES

.PHONY: install-mo
install-mo:
	curl -sSL https://git.io/get-mo -o mo
	chmod +x mo
	sudo mv mo /usr/local/bin

.PHONY: mo
mo:
	@which mo 1>/dev/null || (echo 'FAILURE: Please call `make install-mo` first' && exit 1)

package.json: package.mo.json ../VERSION mo
	VERSION=$(VERSION) ESLINT_VERSION=$(ESLINT_VERSION) mo $< > $@

runner_package.json: runner_package.mo.json ../VERSION mo
	VERSION=$(VERSION) ESLINT_VERSION=$(ESLINT_VERSION) mo $< > $@

.PHONY: clean
clean:
	rm -f package.json

# TODO: build2 -> build, when ready
.PHONY: build2
build2: build2-js_deps build2-stdlib2 build2-react_runner build2-runner

.PHONY: build2-js_deps
build2-js_deps:
	(cd js_deps && $(MAKE) build)

.PHONY: build2-stdlib2
build2-stdlib2:
	docker build --tag=$(STDLIB2_IMAGE) \
	  --file Dockerfile.stdlib2 \
	  --build-arg NODE_VERSION=$(NODE_VERSION) \
	  --build-arg REACH_VERSION=$(VERSION) \
	  --build-arg REACH_GIT_HASH="$$(../scripts/git-hash.sh)" \
		.
	TAG_ONLY=1 ../scripts/docker-push.sh $(STDLIB2_IMAGE)

.PHONY: build2-react_runner
build2-react_runner:
	(cd react_runner && $(MAKE) build)

.PHONY: build2-runner
build2-runner:
	echo TODO build2-runner

# TODO: push more things
# TODO: push2 -> push, when ready
.PHONY: push2
push2: push2-react_runner

.PHONY: push2-react_runner
push2-react_runner:
	(cd react_runner && $(MAKE) push)

.PHONY: build
build: build-stdlib build-runner

.PHONY: build-stdlib
build-stdlib: package.json
	docker build --tag=$(IMAGE):latest \
	  --build-arg NODE_VERSION=$(NODE_VERSION) \
	  --build-arg ESLINT_VERSION=$(ESLINT_VERSION) \
	  --build-arg REACH_GIT_HASH="$$(../scripts/git-hash.sh)" \
	  .
	TAG_ONLY=1 ../scripts/docker-push.sh $(IMAGE)

.PHONY: build-runner
build-runner: runner_package.json
	docker build --tag=$(RUNNER_IMAGE):latest \
	  --file Dockerfile.runner \
	  --build-arg REACH_VERSION=$(VERSION) \
	  .
	TAG_ONLY=1 ../scripts/docker-push.sh $(RUNNER_IMAGE)

.PHONY: push
push:
	../scripts/docker-push.sh $(IMAGE)
	../scripts/docker-push.sh $(RUNNER_IMAGE)

.PHONY: publish
publish:
	npm publish --access public

.PHONY: run
run:
	docker-compose run stdlib

.PHONY: down
down:
	docker-compose down

.PHONY: test
test: run

.PHONY: format
format: package.json
	npm run beautify
	npm run format
