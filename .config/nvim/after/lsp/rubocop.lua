---@type vim.lsp.Config
return {
  -- cmd = { 'bundle', 'exec', 'rubocop', '--lsp' },
  cmd = { 'rubocop', '--lsp' },
  filetypes = { 'ruby' },
  root_markers = { 'Gemfile', '.git' },
}
