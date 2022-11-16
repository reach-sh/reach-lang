IMG_HASKELL_BUILD_ARTIFACTS="reachsh/haskell-build-artifacts"
IMG_REACHC="reachsh/reach"
IMG_REACH_CLI="reachsh/reach-cli"
ROOT=..

include $(ROOT)/VERSION
include $(ROOT)/DEPS
include Makefile.docker

ifeq ($(wildcard $(CLOSED_YAML)),)
	MEVEREST=
else
	MEVEREST=--flag reach:everest
endif

.PHONY: expand
expand: package.yaml src/Reach/Version.hs sol/stdlib.sol

.PHONY: clean
clean: hs-clean
	rm -f package.yaml

.PHONY: build-haskell-build-artifacts-open
build-haskell-build-artifacts-open:
	touch package.closed.yaml
	mkdir -p app-closed
	$(ROOT)/scripts/build-image.sh $(IMG_HASKELL_BUILD_ARTIFACTS)-open Dockerfile.haskell-build-artifacts

.PHONY: build-haskell-build-artifacts-closed
build-haskell-build-artifacts-closed:
	REACH_OPEN=N $(ROOT)/scripts/build-image.sh $(IMG_HASKELL_BUILD_ARTIFACTS)-closed Dockerfile.haskell-build-artifacts

.PHONY: build-reach-cli
build-reach-cli:
	$(ROOT)/scripts/build-image.sh $(IMG_REACH_CLI) Dockerfile.reach-cli
	TAG_ONLY=1 ../scripts/docker-push.sh $(IMG_REACH_CLI)

.PHONY: build-reach
build-reach:
	$(ROOT)/scripts/build-image.sh $(IMG_REACHC) Dockerfile.reachc
	TAG_ONLY=1 ../scripts/docker-push.sh $(IMG_REACHC)

.PHONY: build
build: build-haskell-build-artifacts-open build-reach-cli build-reach

.PHONY: build-m1
build-m1:
	mkdir -p .docker-root
	cp -f $(ROOT)/VERSION $(ROOT)/DEPS .docker-root/
	docker build \
	  --platform linux/arm64 \
	  --build-arg REACH_GIT_HASH="$$(../scripts/git-hash.sh)" \
	  --build-arg SOLC_IMAGE="$(SOLC_IMAGE)" \
	  --build-arg Z3_VERSION="$(Z3_VERSION)" \
	  --build-arg UBUNTU_IMAGE="$(UBUNTU_IMAGE)" \
	  --pull=false \
	  --tag=$(IMG_REACHC):latest \
	  --target=reachc \
	  -f Dockerfile.m1 .
	docker build \
	  --platform linux/arm64 \
	  --build-arg REACH_GIT_HASH="$$(../scripts/git-hash.sh)" \
	  --build-arg SOLC_IMAGE="$(SOLC_IMAGE)" \
	  --build-arg Z3_VERSION="$(Z3_VERSION)" \
	  --build-arg UBUNTU_IMAGE="$(UBUNTU_IMAGE)" \
	  --pull=false \
	  --tag=$(IMG_REACH_CLI):latest \
	  --target=reach-cli \
	  -f Dockerfile.m1 .
	TAG_ONLY=1 ../scripts/docker-push.sh $(IMG_REACHC)
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
	stack build --fast $(MEVEREST)
	stack test --fast --no-run-tests
	$(MAKE) docker-check

.PHONY: hs-clean
hs-clean: expand
	stack clean
	rm -rf .hie

.PHONY: hs-build
hs-build: expand
	stack build --fast --test --no-run-tests $(MEVEREST) reach:test:reach-test reach:exe:reachc reach:exe:reach

.PHONY: hs-build-all
hs-build-all: expand
	stack build --fast --test --no-run-tests $(MEVEREST)

.PHONY: hs-test
hs-test: hs-build
	# these are gitignored, so just delete them
	rm -f $(ROOT)/examples/init/index.{rsh,mjs,txt}
	stack test --fast $(MEVEREST) --test-arguments '+RTS -N -RTS $(HS_TEST_ARGS)'

.PHONY: hs-test-xml
hs-test-xml: hs-build
	mkdir -p test-reports
	$(MAKE) MAYBE_FAST=--fast docker-test-xml

.PHONY: hs-test-accept
hs-test-accept: hs-build
	stack test --fast $(MEVEREST) --test-arguments '+RTS -N -RTS --accept $(HS_TEST_ARGS)'

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

define w
ghcid -c 'stack ghci $(MEVEREST) --main-is=reach:$(1)'
endef

.PHONY: watch
watch: expand
	@$(call w,test:reach-test)

.PHONY: watch-reach-cli
watch-reach-cli: expand
	@REACH_EX=../reach $(call w,exe:reach) -T Main.main

.PHONY: watch-reachc
watch-reachc: expand
	@$(call w,exe:reachc)

.PHONY: watch-tsa
watch-tsa: expand
	@$(call w,exe:tsa)

.PHONY: watch-proto-stub
watch-proto-stub: expand
	@$(call w,exe:proto-stub) -T Main.main

.PHONY: watch-proto-test
watch-proto-test: expand
	@$(call w,test:reach-test) -T Reach.Test_Proto.main

.PHONY: watch-reachpc
watch-reachpc: expand
	@$(call w,exe:reachpc)

.PHONY: repl-reach-cli
repl-reach-cli: expand
	@stack build --fast $(MEVEREST)
	@stack ghci app/reach/Main.hs $(MEVEREST) \
	  --package pretty-simple \
	  --ghci-options "-interactive-print=Text.Pretty.Simple.pPrint"

.PHONY: proto-stub-serve
proto-stub-serve: hs-build
	@stack exec -- proto-stub serve

.PHONY: proto-stub-client
proto-stub-client: hs-build
	@#stack exec -- proto-stub cmd init --help
	@#stack exec -- proto-stub cmd no-such --cli || :
	@stack exec -- proto-stub cmd compile --verify-fail-once
