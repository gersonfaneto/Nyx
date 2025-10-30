---@type lsp.config
return {
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
        command = { 'alejandra' },
      },
    },
  },
}
