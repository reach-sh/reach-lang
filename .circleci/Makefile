.DEFAULT_GOAL := validate

.PHONY: build
build:
	@echo "(Re-)building..."
	@. ../VERSION && export MAJOR MINOR PATCH VERSION && ./rebuild.hs
	@echo

.PHONY: validate
validate: build
	@circleci config validate config.yml
	@echo

.PHONY: do-watch
do-watch:
	@clear
	@echo "Changes will trigger the \`$(TARGET)\` target"
	@echo
	@while inotifywait -e modify \
	    Makefile \
	    .gitignore \
	    config.dhall \
	    rebuild.hs \
	  ; do clear; $(MAKE) -s $(TARGET); done

.PHONY: watch
watch:
	@TARGET=build $(MAKE) -s do-watch

.PHONY: watch-validate
watch-validate:
	@TARGET=validate $(MAKE) -s do-watch
