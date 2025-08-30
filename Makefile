update:
	sudo nixos-rebuild switch --flake .
	# sudo home-manager switch --flake .

format:
	find -type f -name '*.nix' -not -name 'hardware-configuration.nix' \
		-exec nix fmt --quiet {} \;
