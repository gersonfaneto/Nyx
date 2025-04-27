require('utils.lsp').start({
  name = 'hls',
  cmd = { 'haskell-language-server-wrapper', '--lsp' },
  root_patterns = {
    'hie.yaml',
    'stack.yaml',
    'cabal.project',
    '*.cabal',
    'package.yaml',
  },
  settings = {
    haskell = {
      formattingProvider = 'ormolu',
      cabalFormattingProvider = 'cabalfmt',
    },
  },
})
