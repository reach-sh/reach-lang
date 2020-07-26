TAG="reachsh/reach:v0.1.0"
NAME="reach"
CIRCLE_TAG="reachsh/reach-circle:v0.1.0"

.PHONY: build
build:
	docker build --tag=$(TAG) .

.PHONY: push
push:
	docker push $(TAG)

.PHONY: publish
publish:
	echo Go to https://hackage.haskell.org/packages/upload

# TODO: dockerized stan so that make check does not require local hs install?
.PHONY: check
check: hs-check

.PHONY: hs-check
hs-check:
	stack build
	stack test --no-run-tests
	stack build stan
	stack exec stan report

.PHONY: hs-clean
hs-clean:
	stack clean
	rm -rf .hie

.PHONY: hs-build
hs-build:
	stack build

.PHONY: hs-test
hs-test:
	stack test

.PHONY: hs-test-xml
hs-test-xml:
	mkdir -p test-reports
	stack test --test-arguments '--xml=test-reports/junit.xml'

.PHONY: hs-test-html
hs-test-html:
	mkdir -p test-reports
	stack test --test-arguments '--html=test-reports/results.html'

.PHONY: hs-test-accept
hs-test-accept:
	stack test --test-arguments '--accept'

.PHONY: hs-deps
hs-deps:
	stack setup
	stack build --dependencies-only
	stack test --dependencies-only
	stack build stan

.PHONY: hs-doc
hs-doc:
	stack haddock

.PHONY: hs-format
hs-format:
	stack build ormolu
	stack exec -- ormolu --mode inplace $$(find . -name '*.hs')

.PHONY: build-circle-docker
build-circle-docker:
	docker build -f Dockerfile_circleci --tag $(CIRCLE_TAG) .

.PHONY: push-circle-docker
push-circle-docker:
	docker push $(CIRCLE_TAG)
