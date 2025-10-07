CC ?= cc

CFLAGS := -Wall -Wextra -Wpedantic -Werror

CFLAGS += \
	  -Wno-macro-redefined

CFLAGS += -std=gnu99
CFLAGS += -Os

CFLAGS += -I include/


.PHONY: help
help:
	@echo "Usage: make [compdb:build:run|clean]"


.PHONY: build
build:
	@[[ ! -d ./dist/ ]] && mkdir ./dist/ || exit 0
	@cc -o ./dist/main ./src/main.c $(CFLAGS)


.PHONY: run
run:
	@[[ -f ./dist/main ]] && (./dist/main; exit 0) || (echo "Do 'make build' first!"; exit 0)


.PHONY: compdb
compdb:
	@[[ ! -d ./.build/ ]] && mkdir ./.build/ || exit 0
	@bear --output ./.build/compile_commands.json -- make build


.PHONY: clean
clean:
	rm -f -r -v ./.build ./dist
