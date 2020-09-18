include ../VERSION
IMAGE="reachsh/stdlib"
RUNNER_IMAGE="reachsh/runner"
NODE_VERSION="12.18.3"
ESLINT_VERSION="7.7.0"

package.json: package.mo.json ../VERSION 
	VERSION=$(VERSION) ESLINT_VERSION=$(ESLINT_VERSION) mo $< > $@

runner_package.json: runner_package.mo.json ../VERSION 
	VERSION=$(VERSION) ESLINT_VERSION=$(ESLINT_VERSION) mo $< > $@

.PHONY: clean
clean:
	rm -f package.json

.PHONY: build
build: build-stdlib build-runner

.PHONY: build-stdlib
build-stdlib: package.json shared.mjs loader.mjs ETH.mjs FAKE.mjs ALGO.mjs ask.mjs
	$(MAKE) format
	sbin/fix_imports.sh

	docker build --tag=$(IMAGE):latest \
	  --build-arg NODE_VERSION=$(NODE_VERSION) \
	  --build-arg ESLINT_VERSION=$(ESLINT_VERSION) \
	  .
	TAG_ONLY=1 ../scripts/docker-push.sh $(IMAGE)
# TODO: figure out the import weirdness
# TODO: drop .mjs files in repo, push `npm run tsc` into the container

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

%.mjs: build/%.js
	cp $^ $@

build/%.js: %.ts tsconfig.json package.json
	npm run tsc
