REACH = ../../reach
NODE = node
NPM = npm

.PHONY: build
build:
	$(NPM) install
	$(REACH) version
	pwd
	ls
	$(REACH) compile
	$(REACH) run
	$(REACH) compile --sim  & echo $!


.PHONY: run
run:
	$(NODE) client.mjs

.PHONY: clean
clean:
	cd .
