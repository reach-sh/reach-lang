TAG="reachsh/stdlib:v0.1.0"

.PHONY: build
build:
	docker build --tag=$(TAG) .

.PHONY: push
push: build
	docker push $(TAG)
