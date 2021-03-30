.DEFAULT_GOAL := build

.PHONY: compare
compare:
	yaml-to-dhall --file orig.yml > orig.dhall
	dhall diff ./config.dhall ./orig.dhall

.PHONY: build
build:
	@dhall-to-yaml \
	  --generated-comment \
	  --explain \
	  --file ./config.dhall \
	  | tee config.yml
	@echo

.PHONY: watch
watch:
	@clear
	@while inotifywait -e modify config.dhall; do \
	  clear; \
	  $(MAKE) -s build; \
	  done
