IMAGE="reachsh/reach"
CIRCLE_IMAGE="reachsh/reach-circle"

package.yaml: package.mo.yaml ../VERSION
	(source ../VERSION && export VERSION && mo $< > $@)

.PHONY: stack
stack: package.yaml

.PHONY: clean
clean: hs-clean
	rm -f package.yaml

.PHONY: build
build: stack
	docker build --tag=$(IMAGE):latest .

.PHONY: push
push: build
	../scripts/docker-push.sh $(IMAGE)

.PHONY: build-circle-docker
build-circle-docker: stack
	docker build -f Dockerfile_circleci --tag=$(CIRCLE_IMAGE):latest .

.PHONY: push-circle-docker
push-circle-docker: build-circle-docker
	../scripts/docker-push.sh $(CIRCLE_IMAGE)

# TODO: dockerized stan so that make check does not require local hs install?
.PHONY: check
check: hs-check

.PHONY: hs-check
hs-check: stack
	stack build --fast
	stack test --fast --no-run-tests
	stack build stan
	stack exec stan report

.PHONY: hs-clean
hs-clean: stack
	stack clean
	rm -rf .hie

.PHONY: hs-build
hs-build: stack
	stack build --fast

.PHONY: hs-test
hs-test: stack
	stack test --fast

.PHONY: hs-test-xml
hs-test-xml: stack
	mkdir -p test-reports
	stack test --fast --test-arguments '--xml=test-reports/junit.xml'

.PHONY: hs-test-html
hs-test-html: stack
	mkdir -p test-reports
	stack test --fast --test-arguments '--html=test-reports/results.html'

.PHONY: hs-test-accept
hs-test-accept: stack
	stack test --fast --test-arguments '--accept'

.PHONY: hs-deps
hs-deps: stack
	stack setup
	stack build --dependencies-only
	stack test --dependencies-only
	stack build stan

.PHONY: hs-doc
hs-doc: stack
	stack haddock --fast

.PHONY: hs-format
hs-format: stack
	stack build ormolu
	stack exec -- ormolu \
	  --ghc-opt -XBangPatterns \
	  --ghc-opt -XCPP \
	  --ghc-opt -XTypeApplications \
	  --mode inplace $$(find . -name '*.hs')
