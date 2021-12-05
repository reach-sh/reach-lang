REACH = ../../reach
NODE = node
NPM = npm

.PHONY: build
build:
	$(NPM) install
	$(REACH) compile --sim 2>&1 &

.PHONY: run
run:
	$(NODE) client.mjs

.PHONY: clean
clean:
	cd .
