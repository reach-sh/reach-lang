ROOT = ..

# We can derive Reach's Visual Studio Code extension's
# version using the VERSION file, which can be found
# in the root directory.
include $(ROOT)/VERSION

# the next line is so this Makefile can do arithmetic
SHELL=/bin/bash

# double dollar sign to have the shell do the expansion
# https://stackoverflow.com/a/15978322
VSCE_VER := $$(($$(($$(($(MAJOR)+1))*1000)) + $(MINOR))).$(PATCH).$(DRAFT)

all:
	@echo "Now attempting to publish version $(VSCE_VER)"
	@echo "of Reach's Visual Studio Code extension."
	vsce publish $(VSCE_VER)