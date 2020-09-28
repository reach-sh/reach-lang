IMAGE="reachsh/ethereum-devnet"

.PHONY: build
build:
	docker build --build-arg REACH_GIT_HASH="$$(../git-hash.sh)" --tag=$(IMAGE):latest .
	TAG_ONLY=1 ../docker-push.sh $(IMAGE)

.PHONY: push
push: build
	../docker-push.sh $(IMAGE)

.PHONY: run
run: build
	docker run -t -p 8545:8545 $(IMAGE):latest

.PHONY: status
status:
	@curl -sSf -X POST \
	  -H "Content-Type: application/json" \
	  --data '{"jsonrpc":"2.0", "method": "web3_clientVersion", "params":[], "id":67}' \
	  http://localhost:8545
