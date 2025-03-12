require('utils.lsp').start({
  cmd = { 'docker-langserver', '--stdio' },
  root_patterns = {
    'Dockerfile',
  },
  settings = {
    docker = {
      languageserver = {
        formatter = {
          ignoreMultilineInstructions = true,
        },
      },
    },
  },
})
