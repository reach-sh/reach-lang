IMAGE="reachsh/algorand-devnet"

.PHONY: build
build:
	docker build --tag=$(IMAGE):latest .
	TAG_ONLY=1 ../../scripts/docker-push.sh $(IMAGE)
