ROOT=../..

include $(ROOT)/VERSION
versions.txt: Makefile versions-regen.sh $(ROOT)/version
	MAJOR=$(MAJOR) MINOR=$(MINOR) VERSION=$(VERSION) RC_VERSION=$(RC_VERSION) ./versions-regen.sh

# Only do this _after_ doing the regular non-m1 release
# and _after_ doing routine-build-m1
# and exporting the DATEVER and HASHVER vars
.PHONY: release-m1
release-m1: versions.txt
	./m1-stuff.sh "$$DATEVER" "$$HASHVER"

