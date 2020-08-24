examples = over-minimal tut-1/tut tut-2/tut tut-3/tut tut-4-attack/tut tut-4/tut
all: render check

STYLE=solarizedlight

render:
	raco scribble --dest .. --dest-name docs --htmls reach.scrbl
	@rm -f ../minted-$(STYLE)-style.css
	@cp -f minted.css CNAME ../docs/

clean:
	rm -rf ../docs/

check:
	ag --ignore Makefile xxx

install:
	raco pkg install
	pip install pygments-style-solarized
	cd ../py/ && make install # pygments-reach

.PHONY: run-all-examples
run-all-examples:
	(set -e; for example in $(examples) ; do (cd x/ && ../../reach run $$example) ; done)
