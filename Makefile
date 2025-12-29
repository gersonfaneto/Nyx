.PHONY: help
help:
	@echo 'Usage: make [build|update|prune|format]'

.PHONY: prelude
prelude:
	git add -A

.PHONY: build
build: prelude
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
