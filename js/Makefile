include ../VERSION
IMAGE="reachsh/stdlib"
RUNNER_IMAGE="reachsh/runner"

.PHONY: build
build:
	docker build --tag=$(IMAGE):latest .
	TAG_ONLY=1 ../scripts/docker-push.sh $(IMAGE)
	docker build --tag=$(RUNNER_IMAGE):latest \
	  --file Dockerfile_runner \
	  --build-arg REACH_VERSION_ARG=$(VERSION) \
	  .

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
