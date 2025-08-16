return {
  cmd = { 'clojure-lsp' },
  filetypes = { 'clojure', 'edn' },
  root_markers = {
    'project.clj',
    'deps.edn',
    'bb.edn',
    '.git',
  },
  settings = {
    clojure = {
      format = {
        provider = 'cljfmt',
      },
      lint = {
        lintOnChange = true,
        lintOnSave = true,
        lintOnReplLoad = true,
      },
      repl = {
        type = 'clj',
        autoStart = true,
      },
      project = {
        classpath = {
          command = 'lein',
          args = { 'classpath' },
        },
      },
    },
  },
  single_file_support = true,
  log_level = vim.lsp.protocol.MessageType.Warning,
}
