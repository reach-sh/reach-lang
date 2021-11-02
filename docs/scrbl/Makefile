ROOT=../..
TARGET=build

all: render check

.PHONY: rebuild
rebuild:
	raco scribble --dest . --dest-name $(TARGET) --htmls reach.scrbl 2>&1 | tee scribble.log
	[ -f $(TARGET)/index.html ]
	./add-scripts.sh $(TARGET)

.PHONY: copy
copy:
	@cp -fr minted.css CNAME google*.html reach-pre.js reach-post.js clippy.svg reach.css tooltips.js tooltips.css favicon $(TARGET)/
	@cp -f $(ROOT)/reach $(TARGET)/

.PHONY: render
render: rebuild copy

.PHONY: clean
clean:
	rm -rf $(TARGET)/

.PHONY: check
check:
	ag --ignore Makefile xxx
	grep -v -e reach -e javascript -e go -e python -e makefile scribble.log

.PHONY: install
install:
	raco pkg install
	cd $(ROOT)/pygments/ && make install # pygments-reach

.PHONY: watch-render
watch-render:
	@command -v inotifywait >/dev/null \
	  || (echo 'Unable to find `inotifywait`' && exit 1)
	@while inotifywait -e modify -r .; \
	  do clear \
	   ; date \
	   ; echo 'Re-rendering docs...' \
	   ; $(MAKE) -s render \
	   ; done
