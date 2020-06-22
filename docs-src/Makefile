all: render check

render:
	raco scribble --dest .. --dest-name docs --htmls reach.scrbl
	@rm -f ../minted-default-style.css

check:
	ag xxx

install:
	raco pkg install
