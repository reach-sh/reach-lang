REACH = ../../reach

.PHONY: check
check: expected actual
	diff -u $^

actual: build/index.main.pil
	grep between $^ > $@

build/index.main.pil: index.rsh
	$(REACH) compile --intermediate-files $^

.PHONY: build
build: check

.PHONY: clean
clean:
	rm -f build/index.main.pil
