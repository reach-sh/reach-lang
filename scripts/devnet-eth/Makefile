IMAGE=reachsh/devnet-eth
ROOT=../..
include $(ROOT)/DEPS

.PHONY: build
build:
	cp ../daily-ping.sh .
	$(ROOT)/scripts/build-image.sh $(IMAGE) Dockerfile
	TAG_ONLY=1 $(ROOT)/scripts/docker-push.sh $(IMAGE)

.PHONY: build-m1
build-m1: build

.PHONY: build-devnet-eth
build-devnet-eth: build

.PHONY: push
push:
	$(ROOT)/scripts/docker-push.sh $(IMAGE)

.PHONY: run
run: build
	docker run -t -p 8545:8545 $(IMAGE):latest

.PHONY: status
status:
	@curl -sSf -X POST \
	  -H "Content-Type: application/json" \
	  --data '{"jsonrpc":"2.0", "method": "web3_clientVersion", "params":[], "id":67}' \
	  http://localhost:8545
