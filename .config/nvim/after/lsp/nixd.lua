return {
  settings = {
    nixd = {
      nixpkgs = {
        expr = vim.fs.root(0, { 'shell.nix' }) ~= nil
            and 'import <nixpkgs> { }'
          or string.format(
            'import (builtins.getFlake "%s").inputs.nixpkgs { }',
            vim.fs.root(0, { 'flake.nix' }) or vim.fn.expand('$DOTFILES')
          ),
      },
      formatting = {
        command = { 'alejandra' },
      },
      options = vim.tbl_extend('force', {
        home_manager = {
          expr = string.format(
            '(builtins.getFlake "%s").homeConfigurations.%s.options',
            vim.fn.expand('$DOTFILES'),
            vim.fn.hostname()
          ),
        },
      }, {
        nixos = {
          expr = string.format(
            '(builtins.getFlake "%s").nixosConfigurations.%s.options',
            vim.fn.expand('$DOTFILES'),
            vim.fn.hostname()
          ),
        },
      }),
    },
  },
}
