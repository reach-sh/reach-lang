check: expected actual
	diff -u $^ || exit 1

actual: build/main.main.pl
	grep -B 2 between $^ > $@

build/main.main.pl: main.rsh
	../../reach compile -o build $^

run:
	../../reach run main

clean:
	rm -f build/main.main.pl
