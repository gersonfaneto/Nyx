{
  description = " ~ ❄️ ~ ";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs?ref=nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/release-25.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    stylix.url = "github:nix-community/stylix";
    stylix.inputs.nixpkgs.follows = "nixpkgs";

    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    neovim-nightly-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };


  outputs = inputs @ { self, nixpkgs, nixpkgs-unstable, nixos-hardware, ... }:
    let
      system = "x86_64-linux";

      pkgs = nixpkgs.legacyPackages.${system};

      forAllSystems = nixpkgs.lib.genAttrs [
        "aarch64-linux"
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
    in
    {
      templates = import ./templates;

      formatter = forAllSystems (system: pkgs.nixpkgs-fmt);

      packages = forAllSystems (system: {
        default = nixpkgs.legacyPackages.${system}.hello;
      });

      nixosConfigurations = {
        Nyx = inputs.nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs; };

          modules = [
            ./configuration.nix

            inputs.home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.backupFileExtension = "hm";
              home-manager.users.gerson = ./home.nix;
              home-manager.extraSpecialArgs = { inherit inputs; };
            }
          ];
        };
      };
    };
}
