IMAGE="reachsh/ethereum-devnet"

.PHONY: build
build:
	docker build --tag=$(IMAGE):latest .

.PHONY: push
push: build
	../docker-push.sh $(IMAGE)
