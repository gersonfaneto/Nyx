{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs @ { nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      perSystem = { pkgs, system, ... }: {
        _module.args.pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        formatter = pkgs.nixpkgs-fmt;

        devShells.default = with pkgs;
          mkShell {
            packages = [
              cargo
              rust-analyzer-unwrapped
              rustPackages.clippy
              rustc
              rustfmt
            ];

            RUST_SRC_PATH = rustPlatform.rustLibSrc;

            shellHook =
              /* bash */
              ''
              '';
          };
      };
    };
}
