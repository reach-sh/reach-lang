ROOT=..
include $(ROOT)/js/JS_IMAGES

# faster build for local tests; ignores react_runner
.PHONY: b
	(cd js_deps && $(MAKE) build)
	(cd stdlib && $(MAKE) build)
	(cd runner && $(MAKE) build)

# TODO: delete build2 when ready
.PHONY: build
build:
	(cd js_deps && $(MAKE) build)
	(cd stdlib && $(MAKE) build build2)
	(cd runner && $(MAKE) build build2)
	(cd react_runner && $(MAKE) build)

.PHONY: push
push:
	$(ROOT)/scripts/docker-push.sh $(STDLIB_IMAGE)
	$(ROOT)/scripts/docker-push.sh $(RUNNER_IMAGE)
	$(ROOT)/scripts/docker-push.sh $(REACT_RUNNER_IMAGE)
