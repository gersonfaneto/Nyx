---@type pack.spec
return {
  src = 'https://github.com/sindrets/diffview.nvim',
  data = {
    deps = {
      src = 'https://github.com/nvim-lua/plenary.nvim',
    },
    cmds = {
      'DiffviewOpen',
      'DiffviewFileHistory',
    },
    postload = function()
      local dv = require('diffview')

      dv.setup({
        view = {
          default = {
            disable_diagnostics = true,
            winbar_info = true,
          },
        },
        -- file_panel = {
        --   win_config = {
        --     position = 'right',
        --   },
        -- },
      })
    end,
  },
}
