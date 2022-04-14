.PHONY: clean
clean:
	([ -d ./.tmp ] && rm -r ./.tmp) || :
	find . -type d -name build  -prune                      -exec rm -r "{}" \;
	find . -type d -name .reach -prune                      -exec rm -r "{}" \;
	find . -type f -name .gitignore -not -path ./.gitignore -exec rm    "{}" \;
	find . -type f -name .dockerignore                      -exec rm    "{}" \;
	find . -type f -name Dockerfile                         -exec rm    "{}" \;
	find . -type f -name package.json                       -exec rm    "{}" \;
	find . -type f -name index.mjs                          -exec rm    "{}" \;
	find . -type f -name index.rsh                          -exec rm    "{}" \;

.PHONY: build
build:

.PHONY: run
run:
	@./run.sh
