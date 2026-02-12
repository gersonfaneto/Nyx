.PHONY: help
help:
	@echo 'Usage: make [build|repl|format]'


.PHONY: build
build:
	cabal build


.PHONY: repl
repl:
	cabal repl


.PHONY: format
format:
	find . -type f -name '*.hs' ! \( -path '*dist-newstyle*' -o -path '*xmobarrc*' \)  | paste -sd ' ' | xargs fourmolu --mode inplace
	find . -type f -name '*.hs' ! \( -path '*dist-newstyle*' -o -path '*xmobarrc*' \)  | paste -sd ' ' | xargs stylish-haskell --inplace
