---@type pack.spec
return {
  src = 'https://github.com/sindrets/diffview.nvim',
  data = {
    deps = {
      'https://github.com/nvim-lua/plenary.nvim',
    },
    cmds = {
      'DiffviewOpen',
      'DiffviewFileHistory',
    },
    keys = {
      {
        lhs = '<Leader>dv',
        opts = { desc = 'Open diff view' },
      },
      {
        lhs = '<Leader>dh',
        opts = { desc = 'Open file diff history' },
      },
      {
        lhs = '<Leader>dc',
        opts = { desc = 'Close diff view' },
      },
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

      -- stylua: ignore start
      vim.keymap.set({'n'}, '<Leader>dv', '<Cmd>DiffviewOpen<CR>', { desc = 'Open diff view' })
      vim.keymap.set({'n'}, '<Leader>dh', '<Cmd>DiffviewFileHistory<CR>', { desc = 'Open file diff history' })
      vim.keymap.set({'n'}, '<Leader>dc', '<Cmd>DiffviewClose<CR>', { desc = 'Close diff view' })
      -- stylua: ignore end
    end,
  },
}
