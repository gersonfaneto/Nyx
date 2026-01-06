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

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            uv
          ];
          shellHook =
            /* bash */
            ''
              if [ ! -f pyproject.toml ]; then
                ${pkgs.lib.getExe pkgs.uv} init --author-from git --script main.py

                echo "[tool.basedpyright]\nvenvPath = \".\"\nvenv = \".venv\"" >> pyproject.toml

                ${pkgs.lib.getExe pkgs.uv} add --dev ruff
                ${pkgs.lib.getExe pkgs.uv} add --dev basedpyright
                ${pkgs.lib.getExe pkgs.uv} add --dev ty
              fi

              if [ ! -d .venv ]; then
                ${pkgs.lib.getExe pkgs.uv} venv
              fi
              source .venv/bin/activate

              ${pkgs.lib.getExe pkgs.uv} sync

              echo "Development environment ready!"
            '';
        };
      };
    };
}
