REACH = ../../reach
NODE = node
NPM = npm

.PHONY: build
build:
	$(NPM) install
	$(REACH) compile
	$(REACH) compile --sim  & echo $!

.PHONY: run
run:
	pgrep ghc
	pgrep stack
	pgrep reach
	$(NODE) client.mjs
	lsof -ti tcp:3001 | xargs kill

.PHONY: clean
clean:
	cd .
