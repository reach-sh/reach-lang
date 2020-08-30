IMAGE="reachsh/reach"
CIRCLE_IMAGE="reachsh/reach-circle"

.PHONY: build
build:
	docker build --tag=$(IMAGE):latest .

.PHONY: push
push:
	../scripts/docker-push.sh $(IMAGE)

.PHONY: build-circle-docker
build-circle-docker:
	docker build -f Dockerfile_circleci --tag=$(CIRCLE_IMAGE):latest .

.PHONY: push-circle-docker
push-circle-docker:
	../scripts/docker-push.sh $(CIRCLE_IMAGE)

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
