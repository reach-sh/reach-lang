IMG_HASKELL_BUILD_ARTIFACTS="reachsh/haskell-build-artifacts"
IMG_REACHC="reachsh/reach"
IMG_REACH_CLI="reachsh/reach-cli"
ROOT=..

include $(ROOT)/VERSION
include $(ROOT)/DEPS
include Makefile.docker

.PHONY: expand
expand: package.yaml src/Reach/Version.hs sol/stdlib.sol

.PHONY: clean
clean: hs-clean
	rm -f package.yaml

.PHONY: build-haskell-build-artifacts
build-haskell-build-artifacts:
	mkdir -p .docker-root
	cp -f $(ROOT)/VERSION $(ROOT)/DEPS .docker-root/
	$(ROOT)/scripts/build-image.sh $(IMG_HASKELL_BUILD_ARTIFACTS) Dockerfile.haskell-build-artifacts
	docker container rm -f repo

.PHONY: build-reach-cli
build-reach-cli:
	$(ROOT)/scripts/build-image.sh $(IMG_REACH_CLI) Dockerfile.reach-cli
	TAG_ONLY=1 ../scripts/docker-push.sh $(IMG_REACH_CLI)

.PHONY: build-reach
build-reach:
	$(ROOT)/scripts/build-image.sh $(IMG_REACHC) Dockerfile.reachc
	TAG_ONLY=1 ../scripts/docker-push.sh $(IMG_REACHC)

.PHONY: build
build: build-haskell-build-artifacts build-reach-cli build-reach

.PHONY: build-m1
build-m1:
	mkdir -p .docker-root
	cp -f $(ROOT)/VERSION $(ROOT)/DEPS .docker-root/
	docker build \
	  --platform linux/arm64 \
	  --build-arg REACH_GIT_HASH="$$(../scripts/git-hash.sh)" \
	  --build-arg SOLC_VERSION="$(SOLC_VERSION)" \
	  --build-arg Z3_VERSION="$(Z3_VERSION)" \
	  --tag=$(IMG_REACHC):latest \
	  -f Dockerfile.reachc-m1 .
	TAG_ONLY=1 ../scripts/docker-push.sh $(IMG_REACHC)
	docker build \
	  --platform linux/arm64 \
	  --build-arg REACH_GIT_HASH="$$(../scripts/git-hash.sh)" \
	  --tag=$(IMG_REACH_CLI):latest \
	  -f Dockerfile.reach-cli-m1 .
	TAG_ONLY=1 ../scripts/docker-push.sh $(IMG_REACH_CLI)

.PHONY: push
push:
	../scripts/docker-push.sh $(IMG_REACHC)
	../scripts/docker-push.sh $(IMG_REACH_CLI)

# TODO: dockerized stan so that make check does not require local hs install?
.PHONY: check
check: hs-check

.PHONY: hs-check
hs-check: expand
	stack build --fast
	stack test --fast --no-run-tests
	$(MAKE) docker-check

.PHONY: hs-clean
hs-clean: expand
	stack clean
	rm -rf .hie

.PHONY: hs-build
hs-build: expand
	stack build --fast

.PHONY: hs-test
hs-test: hs-build
	# these are gitignored, so just delete them
	rm -f $(ROOT)/examples/init/index.{rsh,mjs,txt}
	stack test --fast --test-arguments '+RTS -N -RTS'

.PHONY: hs-test-xml
hs-test-xml: hs-build
	mkdir -p test-reports
	$(MAKE) MAYBE_FAST=--fast docker-test-xml

.PHONY: hs-test-html
hs-test-html: hs-build
	mkdir -p test-reports
	stack test --fast --test-arguments '--html=test-reports/results.html'

.PHONY: hs-test-accept
hs-test-accept: hs-build
	stack test --fast --test-arguments '+RTS -N -RTS --accept'

.PHONY: hs-deps
hs-deps: expand
	stack setup
	@# Broken-out dependencies help prevent CI resource exhaustion
	RESOLVER=$$(echo "$$(cat stack.yaml | grep 'resolver:')" | sed 's/resolver:\ \s*//') \
	  && stack --resolver "$$RESOLVER" build aeson \
	  && stack --resolver "$$RESOLVER" build yaml \
	  && stack --resolver "$$RESOLVER" build clay \
	  && stack --resolver "$$RESOLVER" build megaparsec \
	  && stack --resolver "$$RESOLVER" build relude \
	  && stack --resolver "$$RESOLVER" build tomland
	stack build --dependencies-only
	stack test --dependencies-only
	stack build stan

.PHONY: hs-doc
hs-doc: hs-build
	stack haddock --fast

.PHONY: hs-format
hs-format: expand
	stack build ormolu
	stack exec -- ormolu \
	  --ghc-opt -XBangPatterns \
	  --ghc-opt -XCPP \
	  --ghc-opt -XTypeApplications \
	  --mode inplace $$(find . -name '*.hs' | grep -v '.mo')

.PHONY: watch
watch: expand
	@ghcid --target=reach:test:reach-test

.PHONY: watch-reach-cli
watch-reach-cli: expand
	@REACH_EX=../reach ghcid --target=reach:exe:reach -T Main.main

.PHONY: watch-reachc
watch-reachc: expand
	@ghcid --target=reach:exe:reachc

.PHONY: repl-reach-cli
repl-reach-cli: expand
	@stack ghci app/reach/Main.hs \
	  --package pretty-simple \
	  --ghci-options "-interactive-print=Text.Pretty.Simple.pPrint"
