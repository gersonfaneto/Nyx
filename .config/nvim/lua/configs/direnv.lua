local direnv = require('direnv')

direnv.setup({
  bin = "direnv",
  autoload_direnv = true,
  statusline = {
    enabled = true,
    icon = "󱚟",
  },
  keybindings = {
    allow = "<Leader>ea",
    deny = "<Leader>ed",
    reload = "<Leader>er",
    edit = "<Leader>ee",
  },
  notifications = {
    level = vim.log.levels.INFO,
    silent_autoload = true,
  },
})
