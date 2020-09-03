all: render check

STYLE=solarizedlight

.PHONY: rebuild
rebuild:
	raco scribble --dest .. --dest-name docs --htmls reach.scrbl
	@rm -f ../minted-$(STYLE)-style.css
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

.PHONY: install
install:
	raco pkg install
	pip3 install pygments-style-solarized
	cd ../py/ && make install # pygments-reach
