help:
	@echo 'Usage: make [system|home|update|format]'

system:
	sudo nixos-rebuild switch --flake .

home:
	nix run home-manager -- switch -b bak --flake .

update:
	sudo nix flake update

format:
	find -maxdepth 1 -type f -name '*.nix' -not -name 'hardware-configuration.nix' -exec nix fmt --quiet {} \;

.PHONY: help system home update format
