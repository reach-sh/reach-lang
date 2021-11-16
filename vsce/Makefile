ROOT = ..

# We can derive Reach's Visual Studio Code extension's
# version using the VERSION file, which can be found
# in the root directory.
include $(ROOT)/VERSION

# the next line is so this Makefile can do arithmetic
# See https://stackoverflow.com/a/15978322.
SHELL=/bin/bash

# double dollar sign to have the shell do the expansion
# See https://stackoverflow.com/a/15978322.
VSCE_VER := $$(($$(($$(($(MAJOR)+1))*1000)) + $(MINOR))).$(PATCH).$(DRAFT)

package.json: base.package.json
# Replace base.package.json with package.json, first, in
# case we updated package.json since we last made
# base.package.json.
	cp package.json base.package.json
	node build.package.json.js

publish:
	@echo "Now attempting to publish version $(VSCE_VER)"
	@echo "of Reach's Visual Studio Code extension."
	vsce publish $(VSCE_VER)