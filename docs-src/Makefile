all: render check

render:
	raco scribble --dest .. --dest-name docs --htmls reach.scrbl
	@rm -f ../minted-default-style.css

check:
	ag --ignore Makefile xxx

install:
	raco pkg install
