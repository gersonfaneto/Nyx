---@type pack.spec
return {
  src = 'https://github.com/nvim-mini/mini.nvim',
  data = {
    postload = function()
      local ms = require('mini.surround')

      ms.setup({
        search_method = 'cover_or_next',
      })

      local mo = require('mini.operators')

      mo.setup({
        evaluate = { prefix = 'go=' },
        replace = { prefix = 'gor' },
        sort = { prefix = 'gos' },
      })
    end,
  },
}
