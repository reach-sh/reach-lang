check: expected actual
	diff -u $^ || exit 1

actual: build/index.main.pl
	grep -B 2 between $^ > $@

build/index.main.pl: index.rsh
	../../reach compile -o build $^

run:
	../../reach run

clean:
	rm -f build/index.main.pl
