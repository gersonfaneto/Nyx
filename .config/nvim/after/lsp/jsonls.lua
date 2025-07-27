---@type lsp_config_t
return {
  cmd = { 'vscode-json-language-server', '--stdio' },
  filetypes = {
    'json',
    'jsonc',
  },
  root_markers = {
    '.git',
  },
  init_options = {
    provideFormatter = true,
  },
}
