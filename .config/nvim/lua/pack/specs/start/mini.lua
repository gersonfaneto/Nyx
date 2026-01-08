---@type pack.spec
return {
  src = 'https://github.com/nvim-mini/mini.nvim',
  data = {
    postload = function()
      local ms = require('mini.surround')

      ms.setup({})

      local mo = require('mini.operators')

      mo.setup({
        evaluate = { prefix = 'go=' },
        replace = { prefix = 'gor' },
        sort = { prefix = 'gos' },
      })
    end,
  },
}
