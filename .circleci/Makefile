.DEFAULT_GOAL := validate

.PHONY: gather-examples
gather-examples:
	@echo "Gathing examples..."
	@./gather-examples.hs
	@echo

.PHONY: build
build: gather-examples
# TODO forward failed compilation exit code
	@dhall-to-yaml \
	  --generated-comment \
	  --explain \
	  --file ./config.dhall \
	  | tee config.yml
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
	    gather-examples.hs \
	  ; do clear; $(MAKE) -s $(TARGET); done

.PHONY: watch
watch:
	@TARGET=build $(MAKE) -s do-watch

.PHONY: watch-validate
watch-validate:
	@TARGET=validate $(MAKE) -s do-watch
