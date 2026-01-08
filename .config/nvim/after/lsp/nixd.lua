return {
  cmd = { 'nixd' },
  filetypes = { 'nix' },
  root_markers = { 'flake.nix', '.git' },
  settings = {
    nixd = {
      nixpkgs = {
        expr = vim.fs.root(0, { 'shell.nix' }) ~= nil
            and 'import <nixpkgs> { }'
          or string.format(
            'import (builtins.getFlake "%s").inputs.nixpkgs { }',
            vim.fs.root(0, { 'flake.nix' }) or vim.env.DOTFILES
          ),
      },
      options = {
        nixos = {
          expr = string.format(
            '(builtins.getFlake "%s").nixosConfigurations.%s.options',
            vim.fs.root(0, { 'flake.nix' })
              or vim.env.DOTFILES,
            vim.fn.hostname()
          ),
        },
        home_manager = {
          expr = string.format(
            '(builtins.getFlake "%s").homeConfigurations.%s.options',
            vim.fs.root(0, { 'flake.nix' }) or vim.env.DOTFILES,
            vim.fn.hostname()
          ),
        },
      },
      formatting = {
        command = { 'nixpkgs-fmt' },
      },
    },
  },
}
