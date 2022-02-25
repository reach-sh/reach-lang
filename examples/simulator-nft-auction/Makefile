REACH = ../../reach
NODE = node
NPM = npm

.PHONY: build
build:
	$(NPM) install
	cp ../nft-auction/index.rsh .

.PHONY: run
run:
	$(REACH) compile --sim &
	$(NODE) client.mjs

.PHONY: clean
clean:
	cd .
