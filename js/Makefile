include ../VERSION
IMAGE="reachsh/stdlib"
RUNNER_IMAGE="reachsh/runner"

package.json: package.mo.json ../VERSION 
	(. ../VERSION && export VERSION && mo $< > $@)

.PHONY: clean
clean:
	rm -f package.json

.PHONY: build
build: build-stdlib build-runner

.PHONY: build-stdlib
build-stdlib: package.json
	docker build --tag=$(IMAGE):latest .
	TAG_ONLY=1 ../scripts/docker-push.sh $(IMAGE)

.PHONY: build-runner
build-runner:
	docker build --tag=$(RUNNER_IMAGE):latest \
	  --file Dockerfile.runner \
	  --build-arg REACH_VERSION_ARG=$(VERSION) \
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
format:
	js-beautify --indent-size 2 --end-with-newline --replace *.mjs
	eslint --fix *.mjs
