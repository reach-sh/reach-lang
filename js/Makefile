.DEFAULT_GOAL := build

ROOT=..
include $(ROOT)/js/JS_IMAGES
include $(ROOT)/DEPS

.PHONY: build-js-deps
build-js-deps:
	(cd js-deps && $(MAKE) build)

.PHONY: build-stdlib
build-stdlib:
	(cd stdlib && $(MAKE) build)

.PHONY: build-runner
build-runner:
	(cd runner && $(MAKE) build)

.PHONY: build-rpc-server
build-rpc-server:
	(cd rpc-server && $(MAKE) build)

.PHONY: build-react-runner
build-react-runner:
	(cd react-runner && $(MAKE) build)

.PHONY: build
build: build-js-deps build-stdlib build-runner build-rpc-server build-react-runner

.PHONY: build-m1
build-m1: build

# faster build for local tests; ignores react-runner
.PHONY: b
b: build-js-deps build-stdlib build-runner build-rpc-server

.PHONY: test
test: build
	(cd stdlib && $(MAKE) check)

# No need to push js-deps or stdlib
.PHONY: push
push:
	$(ROOT)/scripts/docker-push.sh $(RUNNER_IMAGE)
	$(ROOT)/scripts/docker-push.sh $(REACT_RUNNER_IMAGE)
	$(ROOT)/scripts/docker-push.sh $(RPC_SERVER_IMAGE)
