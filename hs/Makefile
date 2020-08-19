NAME="reach"
IMAGE="reachsh/$(NAME)"
TAG_TAG="$(IMAGE):stable"
TAG_MAJOR="$(IMAGE):v0"
TAG_MINOR="$(TAG_MAJOR).1"
TAG_EXACT="$(TAG_MINOR).0"

CIRCLE_TAG="reachsh/reach-circle:v0.1.0"

.PHONY: build
build:
	docker build -t $(TAG_EXACT) -t $(TAG_MINOR) -t $(TAG_MAJOR) -t $(TAG_TAG) .

.PHONY: push
push:
	docker push $(TAG_EXACT) $(TAG_MINOR) $(TAG_MAJOR) $(TAG_TAG)

.PHONY: publish
publish:
	echo Go to https://hackage.haskell.org/packages/upload

# TODO: dockerized stan so that make check does not require local hs install?
.PHONY: check
check: hs-check

.PHONY: hs-check
hs-check:
	stack build --fast
	stack test --fast --no-run-tests
	stack build stan
	stack exec stan report

.PHONY: hs-clean
hs-clean:
	stack clean
	rm -rf .hie

.PHONY: hs-build
hs-build:
	stack build --fast

.PHONY: hs-test
hs-test:
	stack test --fast

.PHONY: hs-test-xml
hs-test-xml:
	mkdir -p test-reports
	stack test --fast --test-arguments '--xml=test-reports/junit.xml'

.PHONY: hs-test-html
hs-test-html:
	mkdir -p test-reports
	stack test --fast --test-arguments '--html=test-reports/results.html'

.PHONY: hs-test-accept
hs-test-accept:
	stack test --fast --test-arguments '--accept'

.PHONY: hs-deps
hs-deps:
	stack setup
	stack build --dependencies-only
	stack test --dependencies-only
	stack build stan

.PHONY: hs-doc
hs-doc:
	stack haddock --fast

.PHONY: hs-format
hs-format:
	stack build ormolu
	stack exec -- ormolu \
	  --ghc-opt -XBangPatterns \
	  --ghc-opt -XCPP \
	  --ghc-opt -XTypeApplications \
	  --mode inplace $$(find . -name '*.hs')

.PHONY: build-circle-docker
build-circle-docker:
	docker build -f Dockerfile_circleci --tag $(CIRCLE_TAG) .

.PHONY: push-circle-docker
push-circle-docker:
	docker push $(CIRCLE_TAG)
