IMAGE=reachsh/devnet-cfx
ROOT=../..
DOCKER_LAYERS="build"
include $(ROOT)/DEPS

.PHONY: build
build-devnet-cfx:
	$(ROOT)/scripts/build_image.sh \
		$(IMAGE) \
		$(DOCKER_LAYERS) \
		Dockerfile \
    	--build-arg REACH_GIT_HASH="$$(../git-hash.sh)";
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
