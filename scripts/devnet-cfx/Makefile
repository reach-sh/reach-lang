IMAGE=reachsh/devnet-cfx
ROOT=../..
include $(ROOT)/DEPS

.PHONY: build
build:
	cp ../daily-ping.sh .
	$(ROOT)/scripts/build-image.sh $(IMAGE) Dockerfile
	TAG_ONLY=1 $(ROOT)/scripts/docker-push.sh $(IMAGE)

.PHONY: build-m1
build-m1: build

.PHONY: build-devnet-cfx
build-devnet-cfx: build

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

.PHONY: faucet-run
faucet-run: build
	docker run -it -p 1337:1337 --rm $(IMAGE):latest

.PHONY: faucet-test
faucet-test:
	curl -vvv 'http://localhost:1337/faucet?address=0x154f5404f5214a12328916f62da1b225754a5939&amount=1000000000000000000'
