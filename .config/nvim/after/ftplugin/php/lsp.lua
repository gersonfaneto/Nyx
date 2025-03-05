require('utils.lsp').start({
  cmd = { 'phpactor', 'language-server' },
  requires = { 'phpactor' },
  root_patterns = {
    'composer.json',
    '.git',
    '.phpactor.json',
    '.phpactor.yml',
  },
})
