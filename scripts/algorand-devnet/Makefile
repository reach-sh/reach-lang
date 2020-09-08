IMAGE="reachsh/algorand-devnet"

.PHONY: build
build:
	docker build --tag=$(IMAGE):latest .
	TAG_ONLY=1 ../../scripts/docker-push.sh $(IMAGE)

.PHONY: run
run: build
	docker run -it -p 18080:8080 $(IMAGE):latest
