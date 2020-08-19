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
