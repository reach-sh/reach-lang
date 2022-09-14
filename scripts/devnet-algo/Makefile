IMAGE=reachsh/devnet-algo
ROOT=../..

include $(ROOT)/DEPS

# When you upgrade Algorand, you don't always need to do generate-network.
# You probably need to upgrade the config.json file. You look at this file...
#
# https://github.com/algorand/go-algorand/blob/v3.8.1-stable/installer/config.json.example
#
# But for the version you want!
# Then you see what is different

.PHONY: build
build:
	if ! [ -d algorand_network ] ; then \
		echo 'Please run `make generate-network` first' ; \
		exit 1 ; \
	fi
	cp ../daily-ping.sh .
	$(ROOT)/scripts/build-image.sh $(IMAGE) Dockerfile
	TAG_ONLY=1 $(ROOT)/scripts/docker-push.sh $(IMAGE)

.PHONY: build-m1
build-m1: build

.PHONY: build-devnet-algo
build-devnet-algo: build

.PHONY: push
push:
	$(ROOT)/scripts/docker-push.sh $(IMAGE)

.PHONY: run
run: build
	docker-compose run --service-ports devnet-algo

.PHONY: run-shell
run-shell: build
	docker run --name algodev --rm -it -p 4180:4180 -p 8980:8980 -p 9392:9392 --entrypoint /bin/bash $(IMAGE):latest

.PHONY: status
status:
	@curl --verbose -s \
		-H "X-Algo-API-Token: $$(cat algorand_data/algod.token)" \
		-d "int 1" \
		localhost:4180/v2/teal/compile

.PHONY: index-status
index-status:
	@curl --verbose -s \
		-H "X-Indexer-API-Token: reach-devnet" \
		localhost:8980/v2/transactions?min-round=0

.PHONY: generate-network
generate-network:
	DOCKER_BUILDKIT=1 docker build --target ignore-generate-out -o out \
		--build-arg ALPINE_IMAGE=${ALPINE_IMAGE} \
		--build-arg GOLANG_IMAGE=${GOLANG_IMAGE} \
		--pull=false \
		.
	rsync -a --progress out/ algorand_network/

.PHONY: clean-network
clean-network:
	rm -rf algorand_network
