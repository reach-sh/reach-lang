REACH = ../../reach

.PHONY: check
check: expected actual
	diff -u $^

actual: build/index.main.pl
	grep between $^ > $@

build/index.main.pl: index.rsh
	$(REACH) compile $^

.PHONY: build
build: check

.PHONY: run
run:
	REACH_CONNECTOR_MODE=ETH $(REACH) run
	REACH_CONNECTOR_MODE=ALGO $(REACH) run

.PHONY: down
down:
	@echo

.PHONY: clean
clean:
	rm -f build/index.main.pl
