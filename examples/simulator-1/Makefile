REACH = ../../reach
NODE = node
NPM = npm

.PHONY: build
build:
	$(NPM) install

.PHONY: run
run:
	REACH_DOCKER=0 $(REACH) compile --sim &
	$(NODE) client.mjs

.PHONY: clean
clean:
	cd .
