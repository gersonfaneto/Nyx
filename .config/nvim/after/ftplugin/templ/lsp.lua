local root_patterns = { 'go.work', 'go.mod', '.git' }

require('utils.lsp').start({
  cmd = { 'templ', 'lsp' },
  requires = { 'templ' },
  root_patterns = root_patterns,
})
