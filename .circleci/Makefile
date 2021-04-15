.DEFAULT_GOAL := validate
LTS := lts-17.8

examples.dhall: gather-examples.hs ../examples
	@echo "Gathing examples..."
	@./gather-examples.hs
	@echo

.PHONY: install-dhall
install-dhall:
	stack --resolver ${LTS} build dhall-yaml

config.yml: install-dhall examples.dhall
	@. ../VERSION && export VERSION && stack --resolver ${LTS} exec -- dhall-to-yaml \
	  --generated-comment \
	  --explain \
	  --file ./config.dhall \
	  > $@

.PHONY: build
build: config.yml

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
