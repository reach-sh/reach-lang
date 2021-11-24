.DEFAULT_GOAL := build

ROOT=../..
REACH=$(ROOT)/reach
include $(ROOT)/VERSION
include $(ROOT)/DEPS

NAME="reach-app-rpc-kitchen-sink-client"
CIMG="reachsh/$(NAME):latest"
RKEY=REACH_RPC_KEY=$$(cat REACH_RPC_KEY.txt)
PORT=REACH_RPC_PORT=3001

.PHONY: clean-client-lib-js
clean-client-lib-js:
	[ -d ./lib ] && rm -r ./lib || true

.PHONY: clean
clean: clean-client-lib-js
	rm -rf build/*.main.mjs

.PHONY: client-lib-js
client-lib-js: clean-client-lib-js
	cp -r $(ROOT)/rpc-client/js ./lib

build/index.main.mjs: $(shell find . -name "*.rsh")
	$(REACH) compile index.rsh main

.PHONY: build
build: client-lib-js build/index.main.mjs
	docker build \
	  --tag=$(CIMG) \
	  --build-arg NODE_VERSION=$(NODE_VERSION) \
	  .

run:
	$(RKEY) $(PORT) $(REACH) rpc-run docker run --rm \
	  -e "REACH_DEBUG" \
	  -e "REACH_RPC_SERVER=host.docker.internal" \
	  -e "REACH_RPC_TLS_REJECT_UNVERIFIED=0" \
	  -e $(RKEY) \
	  -e $(PORT) \
	  --add-host "host.docker.internal:172.17.0.1" \
	  --name     $(NAME) \
	  --network  reach-devnet \
	  "$(CIMG)"
