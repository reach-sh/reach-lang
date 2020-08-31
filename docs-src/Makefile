all: render check

STYLE=solarizedlight

render:
	raco scribble --dest .. --dest-name docs --htmls reach.scrbl
	@rm -f ../minted-$(STYLE)-style.css
	@cp -f minted.css CNAME ../docs/
	./add-analytics.sh

clean:
	rm -rf ../docs/

check:
	ag --ignore Makefile xxx

install:
	raco pkg install
	pip3 install pygments-style-solarized
	cd ../py/ && make install # pygments-reach
