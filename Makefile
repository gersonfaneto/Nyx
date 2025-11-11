help:
	@echo 'Usage: make [build|update|format]'

build:
	sudo nixos-rebuild switch --flake .?submodules=1

update:
	sudo nix flake update

format:
	find -maxdepth 1 -type f -name '*.nix' -not -name 'hardware-configuration.nix' -exec nix fmt --quiet {} \;

.PHONY: help system home update format
