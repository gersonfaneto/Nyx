# Based on :: https://github.com/the-nix-way/dev-templates

{
  description = "Home by @gersonfaneto";

  inputs.nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1.*.tar.gz";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in
    {
      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages = with pkgs; [
            # Lua
            lua
            stylua
            lua-language-server
            luajitPackages.luacheck

            # Bash
            shfmt
            bash-language-server

            # JSON
            vscode-langservers-extracted

            # General
            efm-langserver
          ];
        };
      });
    };
}
