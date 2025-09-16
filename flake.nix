{
  description = "Nyx";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    # nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.05";

    apple-fonts.url = "github:Lyndeno/apple-fonts.nix";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };

    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
  };

  outputs = {self, ...} @ inputs: let
    system = "x86_64-linux";

    # lib = nixpkgs.lib;
    pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = [
        inputs.neovim-nightly-overlay.overlays.default
      ];
    };

    apple-fonts = inputs.apple-fonts.packages.${pkgs.system};

    nyx = {
      name = "Gerson Ferreira";
      user = "gerson";
      email = "me@gersonfaneto.dev";
    };

    forAllSystems = inputs.nixpkgs.lib.genAttrs [
      "aarch64-linux"
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];
  in {
    formatter = forAllSystems (
      system: let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
      in
        pkgs.alejandra
    );

    nixosConfigurations = {
      Nyx = inputs.nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {
          inherit apple-fonts;
          inherit nyx;
        };
        modules = [
          ./configuration.nix

          inputs.home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.gerson = ./home.nix;
          }
        ];
      };
    };

    homeConfigurations = {
      gerson = inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [./home.nix];
        extraSpecialArgs = {
          inherit nyx;
        };
      };
    };
  };
}
