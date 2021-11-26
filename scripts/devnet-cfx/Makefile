IMAGE=reachsh/devnet-cfx
ROOT=../..
include $(ROOT)/DEPS

.PHONY: build
build-devnet-cfx-build:
	docker build --target build \
	--build-arg REACH_GIT_HASH="$$(../git-hash.sh)" \
	--tag=$(IMAGE):build-latest \
	--cache-from=reachsh/devnet-cfx:build-${CIRCLE_BRANCH} \
	--cache-from=reachsh/devnet-cfx:build-master .

.PHONY: build
build-devnet-cfx:
	docker build --build-arg REACH_GIT_HASH="$$(../git-hash.sh)" \
	--tag=$(IMAGE):latest \
	--cache-from=reachsh/devnet-cfx:build-${CIRCLE_BRANCH} \
	--cache-from=reachsh/devnet-cfx:build-master \
	--cache-from=reachsh/devnet-cfx:${CIRCLE_BRANCH} \
	--cache-from=reachsh/devnet-cfx:master .
	TAG_ONLY=1 $(ROOT)/scripts/docker-push.sh $(IMAGE)

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
