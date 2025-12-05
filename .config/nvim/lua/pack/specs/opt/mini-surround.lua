---@type pack.spec
return {
  src = 'https://github.com/nvim-mini/mini.nvim',
  data = {
    postload = function()
      local surround = require('mini.surround')

      surround.setup({})
    end,
  },
}
