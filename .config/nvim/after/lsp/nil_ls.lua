---@type lsp_config_t
return {
  cmd = { 'nil', '--stdio' },
  filetypes = { 'nix' },
  root_markers = { 'flake.nix', '.git' },
  settings = {
    ['nil'] = {
      formatting = {
        command = { 'alejandra' },
      },
    },
  },
}
