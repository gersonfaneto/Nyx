---@type vim.lsp.Config
return {
  cmd = { 'bundle', 'exec', 'rubocop', '--lsp' },
  filetypes = { 'ruby' },
  root_markers = { 'Gemfile', '.git' },
}
