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
build-stdlib: package.json shared.mjs loader.mjs ETH.mjs
	docker build --tag=$(IMAGE):latest \
	  --build-arg NODE_VERSION=$(NODE_VERSION) \
	  --build-arg ESLINT_VERSION=$(ESLINT_VERSION) \
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

# TODO: generalize the below
shared.mjs: build/shared.js
	cp build/shared.js shared.mjs
	$(MAKE) format

loader.mjs: build/loader.js
	cp build/loader.js loader.mjs
	$(MAKE) format

ETH.mjs: build/ETH.js loader.mjs shared.mjs
	cp build/ETH.js ETH.mjs
	$(MAKE) format
	sbin/fix_imports.sh ETH.mjs loader shared
# TODO: figure out what's up with imports

build/shared.js: shared.ts tsconfig.json package.json
	npm run tsc

build/loader.js: loader.ts tsconfig.json package.json
	npm run tsc

build/ETH.js: ETH.ts tsconfig.json package.json
	npm run tsc
