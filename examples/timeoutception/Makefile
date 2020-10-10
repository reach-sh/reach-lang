.PHONY: check
check: expected actual
	diff -u $^

actual: build/index.main.pl
	grep -B 2 between $^ | sed 's# /U[^ ]*mal##g' > $@

build/index.main.pl: index.rsh
	../../reach compile $^

.PHONY: build
build: check

.PHONY: run
run:
	../../reach run

.PHONY: down
down:
	@echo

.PHONY: clean
clean:
	rm -f build/index.main.pl
