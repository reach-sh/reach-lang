VER="v0.1.1"
TAG="reachsh/stdlib:$(VER)"
RUNNER_TAG="reachsh/runner:$(VER)"
NAME="stdlib"

.PHONY: build
build:
	docker build --tag=$(TAG) .
	docker build --tag=$(RUNNER_TAG) \
	  --file Dockerfile_runner \
	  --build-arg REACH_VERSION_ARG=$(VER) \
	  .

.PHONY: push
push:
	docker push $(TAG)
	docker push $(RUNNER_TAG)

.PHONY: publish
publish:
	npm publish --access public

.PHONY: run
run:
	docker-compose run $(NAME)

.PHONY: down
down:
	docker-compose down

.PHONY: test
test: run
