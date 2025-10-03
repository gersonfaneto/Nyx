help:
	@echo 'Usage: make [system|home|update|format]'

system:
	sudo nixos-rebuild switch --flake .

home:
	nix run nixpkgs#home-manager -- switch --flake .

update:
	sudo nix flake update

format:
	find -type f -name '*.nix' -not -name 'hardware-configuration.nix' \
		-exec nix fmt --quiet {} \;

.PHONY: help system home update format
