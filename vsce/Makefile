ROOT = ..

# "use ../DEPS to select the node docker image"
include $(ROOT)/DEPS

# We can derive the extension's version number using
# the VERSION file from the appropriate directory.
include $(ROOT)/VERSION

# the next line is so this Makefile can do arithmetic
# See https://stackoverflow.com/a/15978322.
SHELL=/bin/bash

# double dollar sign to have the shell do the expansion
# See https://stackoverflow.com/a/15978322.
VSCE_VER := $$(($$(($$(($(MAJOR)+1))*1000)) + $(MINOR))).$(PATCH).$(DRAFT)

# We don't need this next part for extension publishing!
# This is just to build package.json after getting files
# from the compiler.
package.json: base.package.json
# Replace base.package.json with package.json, first, in
# case we updated package.json since we last made
# base.package.json.
	cp package.json base.package.json
	node build.package.json.js

publish:
	@echo
	@echo "Now attempting to publish version $(VSCE_VER)"
	@echo "of the extension using $(NODE_IMAGE)..."
	@echo
# Give the extension version to the script, so it can,
# in turn, give the appropriate version number to Docker.
# This is easier than passing the VERSION file from this
# directory's parent directory to Docker.
	NODE_IMAGE=${NODE_IMAGE} VER=$(VSCE_VER) \
	./extensionPublishingHelper.sh