REACH = ../../reach

.PHONY: clean
clean:
	rm -rf server/build/*.main.mjs

server/build/%.main.mjs: server/%.rsh
	$(REACH) compile $^ main

.PHONY: client-lib-js
client-lib-js:
	cp -fr ../../js/rpc-client ./client-js

.PHONY: build
build: server/build/index.main.mjs client-lib-js
	cd client-js && docker build -f Dockerfile --tag=reachsh/reach-app-tut-6-rpc-client-js:latest .
	cd client-py && docker build -f Dockerfile --tag=reachsh/reach-app-tut-6-rpc-client-py:latest .
	cd client-go && docker build -f Dockerfile --tag=reachsh/reach-app-tut-6-rpc-client-go:latest .

.PHONY: run
run: test

.PHONY: test
test: build
	sbin/test-on.sh ETH js py go || echo 'FIXME'
#	sbin/test-on.sh ALGO js py go
#	sbin/test-on.sh FAKE js py go

.PHONY: run_old
run_old: build
	docker-compose -f "docker-compose.yml" up --remove-orphans reach-app-tut-6-rpc-server reach-app-tut-6-rpc
