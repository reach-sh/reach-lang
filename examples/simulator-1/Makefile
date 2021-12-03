REACH = ../../reach
NODE = node

.PHONY: build
build:
	$(REACH) compile --sim &

.PHONY: run
run:
	$(NODE) client.mjs

.PHONY: clean
clean:
	$(REACH) down
