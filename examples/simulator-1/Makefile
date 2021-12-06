REACH = ../../reach
NODE = node
NPM = npm

.PHONY: build
build:
	$(NPM) install
	$(REACH) compile
	$(REACH) compile --sim

.PHONY: run
run:
	$(NODE) client.mjs
	lsof -ti tcp:3001 | xargs kill

.PHONY: clean
clean:
	cd .
