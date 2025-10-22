help:
	@echo 'Usage: make [system|home|update|format]'

system:
	nixos-rebuild switch --flake .

home:
	home-manager switch -b bak --flake . 

update:
	nix flake update

format:
	find -type f -name '*.nix' -not -name 'hardware-configuration.nix' \
		-exec nix fmt --quiet {} \;

.PHONY: help system home update format
