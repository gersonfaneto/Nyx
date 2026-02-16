---@type minimal.lsp.config
return {
  cmd = { 'haskell-language-server-wrapper', '--lsp' },
  filetypes = {
    'cabal',
    'haskell',
    'lhaskell',
  },
  root_markers = {
    'hie.yaml',
    'stack.yaml',
    'cabal.project',
    '*.cabal',
    'package.yaml',
  },
  on_attach = function(client)
    client.server_capabilities.documentFormattingProvider = false
    client.server_capabilities.documentRangeFormattingProvider = false
  end,
  settings = {
    haskell = {
      cabalFormattingProvider = 'cabalfmt',
      formattingProvider = 'stylish-haskell',
      plugin = {
        rename = {
          config = {
            crossModule = true,
          },
        },
      },
    },
  },
}
