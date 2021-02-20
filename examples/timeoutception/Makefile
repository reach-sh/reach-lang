REACH = ../../reach

.PHONY: check
check: expected actual
	diff -u $^

actual: build/index.main.pil
	grep between $^ > $@

build/index.main.pil: index.rsh
	$(REACH) compile $^

.PHONY: build
build: check
	$(REACH) compile index.rsh

.PHONY: run
run:
	REACH_CONNECTOR_MODE=ETH $(REACH) run
	REACH_CONNECTOR_MODE=ALGO $(REACH) run

.PHONY: down
down:
	@echo

.PHONY: clean
clean:
	rm -f build/index.main.pil
