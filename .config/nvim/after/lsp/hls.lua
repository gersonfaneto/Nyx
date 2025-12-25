---@type vim.lsp.Config
return {
  cmd = { "haskell-language-server-wrapper", "--lsp", "+RTS", "-M8G", "-RTS" },
  filetypes = { 'haskell', 'lhaskell' },
  root_markers = {
    'hie.yaml',
    'stack.yaml',
    'cabal.project',
    '*.cabal',
    'package.yaml',
  },
  settings = {
    haskell = {
      cabalFormattingProvider = 'cabalfmt',
      -- formattingProvider = 'ormolu',
      -- formattingProvider = 'fourmolu',
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
