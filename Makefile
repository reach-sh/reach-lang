.PHONY: all
all: check run-all

.PHONY: check
check:
	@find hs -name '*.hs' | xargs wc
	@ag --color --ignore ./Makefile --ignore docs-src/Makefile --ignore svg/todo.svg --ignore README.md --ignore-dir docs --ignore '*.dead' '(xxx|todo|fixme)'

.PHONY: todo
todo:
	@$(MAKE) check | aha --black > todo.html

.PHONY: run-all
run-all:
	cd js && $(MAKE) run
	cd examples && $(MAKE) run-all

.PHONY: build-all
build-all:
	cd hs && $(MAKE) build
	cd scripts/ethereum-devnet && $(MAKE) build
	cd js && $(MAKE) build
	cd examples && $(MAKE) clean-all build-all

.PHONY: push-all
push-all:
	cd hs && $(MAKE) push
	cd scripts/ethereum-devnet && $(MAKE) push
	cd js && $(MAKE) push
	cd examples && $(MAKE) push-all

.PHONY: publish-all
publish-all:
	cd hs && $(MAKE) publish
	cd js && $(MAKE) publish
	cd examples && $(MAKE) publish-all

.PHONY: rebuild-and-run-all-examples
rebuild-and-run-all-examples:
	cd examples && time $(MAKE) clean-all build-all run-all
	cd docs-src && make run-all-examples

.PHONY: rbe
rbe: rebuild-and-run-all-examples

.PHONY: docs
docs:
	cd docs-src && $(MAKE)

.PHONY: reach-lint
reach-lint:
	./reach lint docs-src/  # XXX: "bad example" files must still pass lint?
	./reach lint hs/rsh/
	./reach lint hs/test-examples/features  # XXX: no way for linter to allow rebinding _ ?
