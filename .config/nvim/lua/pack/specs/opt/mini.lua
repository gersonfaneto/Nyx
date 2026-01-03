---@type pack.spec
return {
  src = 'https://github.com/nvim-mini/mini.nvim',
  data = {
    postload = function()
      local surround = require('mini.surround')

      surround.setup({})

      local operators = require('mini.operators')

      operators.setup({
        evaluate = { prefix = 'go=' },
        replace = { prefix = 'gor' },
        sort = { prefix = 'gos' },
      })
    end,
  },
}
