---@type vim.lsp.Config
return {
  -- cmd = { 'bundle', 'exec', 'solargraph', 'stdio' },
  cmd = { 'solargraph', 'stdio' },
  filetypes = { 'ruby' },
  root_markers = { 'Gemfile', '.git' },
  on_attach = function(client)
    client.server_capabilities.documentFormattingProvider = false
  end,
  settings = {
    solargraph = {
      autoformat = true,
      completion = true,
      diagnostic = true,
      folding = true,
      references = true,
      rename = true,
      symbols = true,
    },
  },
}
