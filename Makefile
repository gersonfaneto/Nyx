ROOT ?= /mnt

HOST ?= Nyx


.PHONY: help
help:
	@echo 'Usage: make [install|build|update|prune|format]'


.PHONY: install
install:
	sudo nixos-install --root $(ROOT) --flake .#$(HOST)


.PHONY: build
build:
	sudo nixos-rebuild switch --flake .


.PHONY: update
update:
	sudo nix flake update


.PHONY: prune
prune:
	sudo nix-collect-garbage --delete-old


.PHONY: check
check:
	nix flake check


.PHONY: format
format:
	find . -type f -name '*.nix' ! \( -path '*\.emacs\.d*' -o -name 'hardware*.nix' \)  | paste -sd ' ' | xargs nix fmt
