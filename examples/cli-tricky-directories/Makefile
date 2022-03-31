.PHONY: clean
clean:
	([ -d ./.tmp ] && rm -r ./.tmp) || :
	find . -type d -name build -prune -exec rm -r "{}" \;
	find . -type f -name Dockerfile   -exec rm    "{}" \;
	find . -type f -name package.json -exec rm    "{}" \;

.PHONY: build
build:

.PHONY: run
run:
	@./run.sh
