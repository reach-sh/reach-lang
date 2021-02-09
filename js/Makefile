ROOT=..
include $(ROOT)/js/JS_IMAGES

# faster build for local tests; ignores react-runner
.PHONY: b
	(cd js-deps && $(MAKE) build)
	(cd stdlib && $(MAKE) build)
	(cd runner && $(MAKE) build)
	(cd rpc-server && $(MAKE) build)
	(cd rpc-client && $(MAKE) build)
	(cd rpc-client-runner && $(MAKE) build)

.PHONY: build
build:
	(cd js-deps && $(MAKE) build)
	(cd stdlib && $(MAKE) build)
	(cd runner && $(MAKE) build)
	(cd rpc-server && $(MAKE) build)
	(cd rpc-client && $(MAKE) build)
	(cd rpc-client-runner && $(MAKE) build)
	(cd react-runner && $(MAKE) build)

.PHONY: test
test: build
	(cd stdlib && $(MAKE) test)

# No need to push js-deps or stdlib
.PHONY: push
push:
	$(ROOT)/scripts/docker-push.sh $(RUNNER_IMAGE)
	$(ROOT)/scripts/docker-push.sh $(REACT_RUNNER_IMAGE)
	$(ROOT)/scripts/docker-push.sh $(RPC_SERVER_IMAGE)
