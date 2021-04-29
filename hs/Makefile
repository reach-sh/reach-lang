IMAGE="reachsh/reach"

.PHONY: install-mo
install-mo:
	curl -sSL https://git.io/get-mo -o mo
	chmod +x mo
	sudo mv mo /usr/local/bin

.PHONY: mo
mo:
	@which mo 1>/dev/null || (echo 'FAILURE: Please call `make install-mo` first' && exit 1)

package.yaml: package.mo.yaml ../VERSION mo
	(. ../VERSION && export VERSION && mo $< > $@)

.PHONY: stack
stack: package.yaml

.PHONY: clean
clean: hs-clean
	rm -f package.yaml

.PHONY: build
build: stack
	docker build --build-arg REACHC_HASH="$$(../scripts/git-hash.sh)" --tag=$(IMAGE):latest .
	TAG_ONLY=1 ../scripts/docker-push.sh $(IMAGE)

.PHONY: push
push:
	../scripts/docker-push.sh $(IMAGE)

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
	stack test --fast --test-arguments '+RTS -N -RTS'

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
	stack test --fast --test-arguments '+RTS -N -RTS --accept'

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

.PHONY: gen-reach
gen-reach: stack
	stack build --fast :gen-reach
	stack exec gen-reach > sbin/reach
	chmod +x sbin/reach

.PHONY: ghcid
ghcid: stack
	@# ghcid --target=reach:test:reach-test -T "Main.main"
	ghcid --target=reach:test:reach-test -T "Reach.Test_Eval.main"
