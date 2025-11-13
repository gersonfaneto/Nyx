.PHONY: help
help:
	@echo 'Usage: make [all|build|update|prune|format]'


.PHONY: all
all: prune format update build
	@echo 'This is going to take a while...'


.PHONY: build
build:
	sudo nixos-rebuild switch --flake .


.PHONY: update
update:
	sudo nix flake update


.PHONY: prune
prune:
	sudo nix-collect-garbage --delete-old


.PHONY: format
format:
	find -type f -name '*.nix' -exec nix fmt --quiet {} \;
