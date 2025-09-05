{
  description = "Nyx";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs?ref=nixos-unstable";

    apple-fonts.url = "github:Lyndeno/apple-fonts.nix";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-unstable,
    home-manager,
    ...
  } @ inputs: let
    system = "x86_64-linux";

    # lib = nixpkgs.lib;
    pkgs = nixpkgs.legacyPackages.${system};
    pkgs-unstable = nixpkgs-unstable.legacyPackages.${system};

    apple-fonts = inputs.apple-fonts.packages.${pkgs.system};

    nyx = {
      name = "Gerson Ferreira";
      user = "gerson";
      email = "me@gersonfaneto.dev";
    };

    forAllSystems = nixpkgs.lib.genAttrs [
      "aarch64-linux"
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];
  in {
    formatter = forAllSystems (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        pkgs.alejandra
    );

    nixosConfigurations = {
      Nyx = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {
          inherit apple-fonts;
          inherit nyx;
          inherit pkgs-unstable;
        };
        modules = [
          ./configuration.nix

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.gerson = ./home.nix;
          }
        ];
      };
    };

    homeConfigurations = {
      gerson = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [./home.nix];
        extraSpecialArgs = {
          inherit nyx;
          inherit pkgs-unstable;
        };
      };
    };
  };
}
