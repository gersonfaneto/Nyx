---@type lsp.config
return vim.g.nix_lsp == 'nil'
    and {
      cmd = { 'nil', '--stdio' },
      filetypes = { 'nix' },
      root_markers = { 'flake.nix', '.git' },
      settings = {
        ['nil'] = {
          nix = {
            flake = {
              autoArchive = true,
            },
          },
          formatting = {
            command = { 'nixpkgs-fmt' },
          },
        },
      },
    }
  or {}

