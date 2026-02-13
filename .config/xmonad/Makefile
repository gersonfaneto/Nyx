FIND_HS_FILES=find . -type f -name '*.hs' ! \( -path '*dist-newstyle*' -o -path '*xmobarrc*' \)  | paste -sd ' '


.PHONY: help
help:
	@echo 'Usage: make [build|repl|format]'


.PHONY: build
build:
	cabal build


.PHONY: repl
repl:
	cabal repl


.PHONY: lint
lint:
	hlint -j lib src


.PHONY: format
format:
	find . -type f -name '*.hs' ! \( -path '*dist-newstyle*' -o -path '*xmobarrc*' \)  | paste -sd ' ' | xargs fourmolu --mode inplace
	find . -type f -name '*.hs' ! \( -path '*dist-newstyle*' -o -path '*xmobarrc*' \)  | paste -sd ' ' | xargs stylish-haskell --inplace
