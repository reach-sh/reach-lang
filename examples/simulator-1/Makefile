REACH = ../../reach
NODE = node

.PHONY: build
build:
	cd .

.PHONY: run
run:
	$(REACH) compile --sim &
	$(NODE) client.mjs

.PHONY: clean
clean:
	$(REACH) down
