RSHES=index.rsh

include ../MAKEFILE_RPC

.PHONY: clean
clean:
	@rm -fr ./lib
	@rm -fr ./tls
	@rm -fr ./build/*.main.mjs
	@rm -f  common.mjs
	@rm -f  Dockerfile
	@rm -f  package.json

$(RSHES):
	@:

build/index.main.mjs:
	@$(REACH) compile index.rsh
