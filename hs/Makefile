TAG="reachsh/reach:v0.1.0"
NAME="reach"

.PHONY: build
build:
	docker build --tag=$(TAG) .

.PHONY: push
push:
	docker push $(TAG)

.PHONY: publish
publish:
	echo Go to https://hackage.haskell.org/packages/upload
