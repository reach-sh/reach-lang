REACH = ../../reach
include ../../VERSION


.PHONY: clean-client-lib-js
clean-client-lib-js:
	[ -d ./client-js/lib ] && rm -r ./client-js/lib || true

.PHONY: clean-client-lib-py
clean-client-lib-py:
	[ -d ./client-py/lib ] && rm -r ./client-py/lib || true

.PHONY: clean-client-lib-go
clean-client-lib-go:
	[ -d ./client-go/lib ] && rm -r ./client-go/lib || true

.PHONY: clean
clean: clean-client-lib-js \
       clean-client-lib-py \
       clean-client-lib-go
	rm -rf server/build/*.main.mjs


server/build/%.main.mjs: server/%.rsh
	$(REACH) compile $^ main


.PHONY: client-lib-js
client-lib-js: clean-client-lib-js
	cp -r ../../rpc-client/js ./client-js/lib

.PHONY: client-lib-py
client-lib-py: clean-client-lib-py
	$(MAKE) -C ../../rpc-client/py build
	cp -r ../../rpc-client/py/dist ./client-py/lib

.PHONY: client-lib-go
client-lib-go: clean-client-lib-go
	cp -r ../../rpc-client/go ./client-go/lib


.PHONY: build
build: server/build/index.main.mjs \
       client-lib-js \
       client-lib-py \
       client-lib-go
	cd client-js && docker build --tag=reachsh/reach-app-tut-7-rpc-client-js:latest .
	cd client-go && docker build --tag=reachsh/reach-app-tut-7-rpc-client-go:latest .
	cd client-py && docker build \
	  --build-arg VERSION=$(shell cat ../../rpc-client/py/VERSION) \
	  --tag=reachsh/reach-app-tut-7-rpc-client-py:latest .

# TODO restore `rpc-server` image in CI
.PHONY: run
run:
	REACH_DEBUG=1 sbin/test-on.sh "${REACH_CONNECTOR_MODE}" js py go
