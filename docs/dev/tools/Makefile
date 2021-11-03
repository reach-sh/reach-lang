.PHONY: build
build: installed
	NODE_ENV=production node generator.js -t all

.PHONY: profile
profile: installed
	NODE_ENV=production node --prof generator.js -t all
	NODE_ENV=production node --prof-process isolate*.log > proj.txt
	rm isolate*log

installed:
	npm install
	touch $@
