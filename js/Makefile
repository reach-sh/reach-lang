ROOT=..
include $(ROOT)/js/JS_IMAGES

# faster build for local tests; ignores react_runner
.PHONY: b
	(cd js_deps && $(MAKE) build)
	(cd stdlib && $(MAKE) build)
	(cd runner && $(MAKE) build)

# TODO: build2 -> build, when ready
.PHONY: build
build:
	(cd js_deps && $(MAKE) build)
	(cd stdlib && $(MAKE) build build2)
	(cd runner && $(MAKE) build build2)
	(cd react_runner && $(MAKE) build)

.PHONY: push
push:
	(cd stdlib && $(MAKE) push)
	(cd runner && $(MAKE) push)
	(cd react_runner && $(MAKE) push)
