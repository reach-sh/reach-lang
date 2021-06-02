# This example is not designed to be used with reach run

IMG=reachsh/reach-example-webpack-5:latest

.PHONY: build
build:
	docker build . -t ${IMG}

# Just runs the node.js program
.PHONY: run
run: build
	docker run ${IMG} js-run-node
	echo 'to serve, use `make serve`'

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

.PHONY: js-build
js-build: dist/bundle.js

# Requires that you `make js-install`
dist/bundle.js: main.js
	npm run build

.PHONY: js-run-node
js-run-node: main.js
	node main.js

# Requires that you `make js-install`
.PHONY: js-run-serve
js-run-serve: dist/bundle.js
	npm run serve
