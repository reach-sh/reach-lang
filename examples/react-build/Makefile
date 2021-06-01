# This example is not designed to be used with reach run

IMG=reachsh/reach-example-react-build:latest

.PHONY: build
build:
	docker build . -t ${IMG}

.PHONY: run
run: build
	echo 'use `make serve` to run me'

.PHONY: serve
serve: build
	docker run --rm -it -p 5000:5000 ${IMG} js-run-serve

# TODO: clean up the below

# Requires that you have npm linked @reach-sh/stdlib
# (cd ../../js/stdlib && make js-build && npm link)
.PHONY: js-install
js-install: package.json
	npm install
	npm link @reach-sh/stdlib

# Requires that you `make js-install`
.PHONY: js-build
js-build:
	npm run build

# You should `make js-build` before doing this
.PHONY: js-run-serve
js-run-serve:
	npm run serve
