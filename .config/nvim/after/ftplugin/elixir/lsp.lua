require('utils.lsp').start({
  cmd = { 'elixir-ls' },
  requires = { 'elixir-ls' },
  root_patterns = {
    'mix.exs',
  }
})
