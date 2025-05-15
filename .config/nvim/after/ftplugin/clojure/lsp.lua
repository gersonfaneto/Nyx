require('utils.lsp').start({
  cmd = { 'clojure-lsp' },
  root_patterns = {
    'project.clj',
    'deps.edn',
    'build.boot',
    'shadow-cljs.edn',
    '.git',
    'bb.edn',
  },
})
