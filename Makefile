.PHONY: all
all: check run-all

.PHONY: check-vers
check-vers:
	ag --ignore '*lock*' --ignore hs/stack.yaml '0\.1\.0' || exit 0
	@echo Should be empty
	ag --ignore '*lock*' --ignore hs/stack.yaml '0\.1\.1' || exit 0
	@echo Should be empty
	ag --ignore '*lock*' --ignore hs/stack.yaml --ignore eslint/package.json --ignore '*rsh' --ignore '*txt' --ignore '*-lock*' '0\.1'

.PHONY: check
check: check-vers
	@find hs -name '*.hs' | xargs wc
	@ag --color --ignore ./Makefile --ignore docs-src/Makefile --ignore svg/todo.svg --ignore README.md --ignore-dir docs --ignore 'package-lock.json' --ignore '*.dead' '(xxx|todo|fixme)'

.PHONY: todo
todo:
	@$(MAKE) check | aha --black > todo.html

.PHONY: run-all
run-all:
	cd js && $(MAKE) run
	cd examples && $(MAKE) run-all

.PHONY: build-all
build-all:
	cd hs && $(MAKE) build build-circle-docker
	cd scripts/devnet-eth && $(MAKE) build
	cd js && $(MAKE) build
	cd examples && $(MAKE) clean-all build-all

.PHONY: push-all
push-all:
	cd hs && $(MAKE) push push-circle-docker
	cd scripts/devnet-eth && $(MAKE) push
	cd js && $(MAKE) push

.PHONY: routine-build-push
routine-build-push:
	scripts/routine.sh build-push

.PHONY: routine-build
routine-build:
	scripts/routine.sh build

.PHONY: rebuild-and-run-all-examples
rebuild-and-run-all-examples:
	(cd js && $(MAKE) build run down)
	(cd examples && time $(MAKE) clean-all build-all run-all)

.PHONY: rbe
rbe: rebuild-and-run-all-examples

.PHONY: docs
docs:
	cd docs-src && $(MAKE)

.PHONY: sh-lint
sh-lint:
	@(set -e; for f in $$(find . -not \( \( -path '*openzeppelin*' -o -path '*/node_modules/*' -o -path '*forks*' \) -prune \) -name '*.sh') ./reach; do \
		echo linting $$f; \
		shellcheck --external-sources --source-path SCRIPTDIR $$f ; \
	done)

# (cd hs && stack install hadolint)
.PHONY: docker-lint
docker-lint:
	find . -not \( -path '*/node_modules/*' -prune \) -name 'Dockerfile*' | xargs hadolint
