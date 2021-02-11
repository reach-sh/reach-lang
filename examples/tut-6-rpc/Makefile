REACH = ../../reach

.PHONY: clean
clean:
	rm -rf server/build/*.main.mjs

server/build/%.main.mjs: server/%.rsh
	$(REACH) compile $^ main

# TODO: ditch this once `@reach-sh/rpc-client` package is available on NPM
.PHONY: client-lib-js
client-lib-js:
	cd ../../js/rpc-client && npm pack
	mv ../../js/rpc-client/reach-sh-rpc-client-0.0.1.tgz ./client-js

.PHONY: build
build: server/build/index.main.mjs client-lib-js
	cd client-js && docker build -f Dockerfile --tag=reachsh/reach-app-tut-6-rpc-client-js:latest .
	cd client-py && docker build -f Dockerfile --tag=reachsh/reach-app-tut-6-rpc-client-py:latest .
	cd client-go && docker build -f Dockerfile --tag=reachsh/reach-app-tut-6-rpc-client-go:latest .

.PHONY: run
run: build
	docker-compose -f "docker-compose.yml" up --remove-orphans reach-app-tut-6-rpc-server reach-app-tut-6-rpc
