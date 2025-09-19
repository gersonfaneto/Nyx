---@type vim.lsp.Config
return {
  cmd = { 'bundle', 'exec', 'solargraph', 'stdio' },
  filetypes = { 'ruby' },
  root_markers = { 'Gemfile', '.git' },
  on_attach = function(client)
    client.server_capabilities.documentFormattingProvider = false
  end,
  init_options = {
    intellisense = false,
    codeCompletion = false,
    formatting = false,
    autoformat = false,
    useBundler = true,
    diagnostics = false,
  },
  settings = {
    solargraph = {
      intellisense = false,
      codeCompletion = false,
      formatting = false,
      autoformat = false,
      useBundler = true,
      diagnostics = false,
    },
  },
}
