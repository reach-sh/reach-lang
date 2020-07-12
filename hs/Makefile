TAG="reachsh/reach:v0.1.0"
NAME="reach"
CIRCLE_TAG="reachsh/reach-circle:v0.1.0"

.PHONY: build
build:
	docker build --tag=$(TAG) .

.PHONY: push
push:
	docker push $(TAG)

.PHONY: publish
publish:
	echo Go to https://hackage.haskell.org/packages/upload

.PHONY: check
check:
	stack build
	stack build stan
	stack exec stan report

.PHONY: clean
clean:
	stack clean
	rm -rf .hie

.PHONY: build-circle-docker
build-circle-docker:
	docker build -f Dockerfile_circleci --tag $(CIRCLE_TAG) .

.PHONY: push-circle-docker
push-circle-docker:
	docker push $(CIRCLE_TAG)
