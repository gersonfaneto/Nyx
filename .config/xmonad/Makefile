GHC_OPTIONS ?=


.PHONY: help
help:
	@echo 'Usage: make [build|repl|lint|format|clean]'


.PHONY: build
build:
	cabal build --ghc-options "$(GHC_OPTIONS)"


.PHONY: repl
repl:
	cabal repl --ghc-options "$(GHC_OPTIONS)"


.PHONY: lint
lint:
	hlint -j lib src


.PHONY: format
format:
	find . -type f -name '*.hs' ! \( -path '*dist-newstyle*' -o -path '*xmobarrc*' \)  | paste -sd ' ' | xargs fourmolu --mode inplace
	find . -type f -name '*.hs' ! \( -path '*dist-newstyle*' -o -path '*xmobarrc*' \)  | paste -sd ' ' | xargs stylish-haskell --inplace


.PHONY: clean
clean:
	git clean -f -d -x .
