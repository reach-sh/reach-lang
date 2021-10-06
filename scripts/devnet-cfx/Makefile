IMAGE=reachsh/devnet-cfx
CONFLUX_VERSION=v1.1.5-testnet

ROOT=../..

.PHONY: build
build:
	docker build --build-arg REACH_GIT_HASH=$$(../git-hash.sh) --build-arg CONFLUX_VERSION=$(CONFLUX_VERSION) --tag=$(IMAGE):latest .
	TAG_ONLY=1 ../docker-push.sh $(IMAGE)

.PHONY: push
push:
	$(ROOT)/scripts/docker-push.sh $(IMAGE)

.PHONY: run
run: build
	docker run -t -p 12537:12537 $(IMAGE):latest

.PHONY: status
status:
	@curl -sSf -X POST \
	  -H "Content-Type: application/json" \
	  --data '{"jsonrpc":"2.0", "method": "cfx_clientVersion", "params":[], "id":67}' \
	  http://localhost:12537
