require('nvim-surround').buffer_setup({
  aliases = {
    ['b'] = { '{', '[', '(', '<', 'b' },
  },
  surrounds = {
    ['b'] = {
      add = { '**', '**' },
      find = '%*%*.-%*%*',
      delete = '^(%*%*)().-(%*%*)()$',
    },
    ['i'] = {
      add = { '_', '_' },
      find = '%_.-%_',
      delete = '^(%_)().-(%_)()$',
    },
  },
})
