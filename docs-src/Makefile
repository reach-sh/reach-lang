all: render check

.PHONY: rebuild
rebuild:
	raco scribble --dest .. --dest-name docs --htmls reach.scrbl 2>&1 | tee scribble.log
	./add-scripts.sh

.PHONY: copy
copy:
	@cp -f minted.css CNAME google*.html reach-pre.js reach-post.js clippy.svg reach.css tooltips.js tooltips.css ../docs/

.PHONY: render
render: rebuild copy

.PHONY: clean
clean:
	rm -rf ../docs/

.PHONY: check
check:
	ag --ignore Makefile xxx
	grep -v -e reach -e javascript scribble.log

.PHONY: install
install:
	raco pkg install
	cd ../py/ && make install # pygments-reach
