REACH = ../../reach

.PHONY: clean
clean:
	rm -rf server/build/*.main.mjs

server/build/%.main.mjs: server/%.rsh
	$(REACH) compile $^ main

.PHONY: build
build: server/build/index.main.mjs
	cd server && docker build -f Dockerfile --tag=reachsh/reach-app-tut-6-rpc-server:latest .
	cd client-js && docker build -f Dockerfile --tag=reachsh/reach-app-tut-6-rpc-client-js:latest .

.PHONY: run
run: build
	docker-compose -f "docker-compose.yml" run --rm reach-app-tut-6-rpc $(ARGS)
