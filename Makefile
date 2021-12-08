.DEFAULT_GOAL := routine-build

.PHONY: check
check:
	ag --ignore '*lock*' --ignore hs/stack.yaml '0\.1\.0' || exit 0
	@echo Should be empty
	ag --ignore '*lock*' --ignore hs/stack.yaml '0\.1\.1' || exit 0
	@echo Should be empty
	ag --ignore '*lock*' --ignore hs/stack.yaml --ignore eslint/package.json --ignore '*rsh' --ignore '*txt' --ignore '*-lock*' '0\.1'
	@find hs -name '*.hs' | xargs wc
	@ag --color --ignore ./Makefile --ignore docs-src/Makefile --ignore svg/todo.svg --ignore README.md --ignore-dir docs --ignore 'package-lock.json' --ignore '*.dead' '(xxx|todo|fixme)'

.PHONY: routine-build-push
routine-build-push:
	scripts/routine.sh build-push

.PHONY: routine-build
routine-build:
	scripts/routine.sh build

.PHONY: routine-build-m1
routine-build-m1:
	scripts/routine.sh build-m1

.PHONY: docs
docs:
	cd docs-src && $(MAKE)

.PHONY: sh-lint
sh-lint:
	@(set -e; for f in \
	    $$(find . \
	      -not \( -path '*openzeppelin*'         -prune \) \
	      -not \( -path '*/node_modules/*'       -prune \) \
	      -not \( -path '*forks*'                -prune \) \
	      -not \( -path './hs/app/reach/embed/*' -prune \) \
	      -name '*.sh') \
	      ./reach; do \
	  echo linting $$f; \
	  shellcheck --external-sources --source-path SCRIPTDIR $$f ; \
	done)
	@(set -e; for f in $$(find ./hs/app/reach/embed -name '*.sh'); do \
	  echo linting $$f; \
	  shellcheck --external-sources --source-path SCRIPTDIR --exclude 2148 $$f ; \
	done)

# (cd hs && stack install hadolint)
.PHONY: docker-lint
docker-lint:
	find . -not \( -path '*/node_modules/*' -prune \) -name 'Dockerfile*' | xargs hadolint
