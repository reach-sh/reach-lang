TAG="reachsh/reach:v0.1.0"
NAME="reach"

.PHONY: build
build:
	docker build --tag=$(TAG) .

.PHONY: push
push: build
	docker push $(TAG)
