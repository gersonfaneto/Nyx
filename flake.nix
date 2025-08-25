{
  description = "Nyx";

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs?ref=nixos-25.05";
    };

    apple-fonts = {
      url = "github:Lyndeno/apple-fonts.nix";
      # inputs = {
      #   nixpkgs.follows = "nixpkgs";
      # };
    };
  };

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: {
    nixosConfigurations = {
      Nyx = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {
          inherit inputs;
        };
        modules = [
          ./configuration.nix
        ];
      };
    };
  };
}
