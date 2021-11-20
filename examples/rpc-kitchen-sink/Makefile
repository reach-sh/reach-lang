.DEFAULT_GOAL := build

ROOT=../..
REACH=$(ROOT)/reach
include $(ROOT)/VERSION
include $(ROOT)/DEPS

NAME="reach-app-rpc-kitchen-sink-client"
CIMG="reachsh/$(NAME):latest"
RKEY=REACH_RPC_KEY=$$(cat REACH_RPC_KEY.txt)

.PHONY: clean-client-lib-js
clean-client-lib-js:
	[ -d ./lib ] && rm -r ./lib || true

.PHONY: clean
clean: clean-client-lib-js
	rm -rf build/*.main.mjs

.PHONY: client-lib-js
client-lib-js: clean-client-lib-js
	cp -r $(ROOT)/rpc-client/js ./lib

build/index.main.mjs: index.rsh
	$(REACH) compile index.rsh main

.PHONY: build
build: client-lib-js build/index.main.mjs
	docker build \
	  --tag=$(CIMG) \
	  --build-arg NODE_VERSION=$(NODE_VERSION) \
	  .

run:
	@# Start fresh devnet to ensure server-log is populated correctly
	@echo "Awaiting RPC server..." \
	  && $(REACH) down \
	  && { REACH_DEBUG=1 $(RKEY) $(REACH) rpc-server >server-log & } \
	  && $(RKEY) $(REACH) rpc-server-await
	@docker run --rm \
	  -e "REACH_DEBUG" \
	  -e "REACH_RPC_SERVER=host.docker.internal" \
	  -e "REACH_RPC_PORT=3000" \
	  -e "REACH_RPC_TLS_REJECT_UNVERIFIED=0" \
	  -e $(RKEY) \
	  --add-host "host.docker.internal:172.17.0.1" \
	  --name     $(NAME) \
	  --network  reach-devnet \
	  "$(CIMG)" || (cat server-log && exit 1)
