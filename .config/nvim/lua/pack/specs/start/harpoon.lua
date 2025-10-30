---@type pack.spec
return {
  src = 'https://github.com/theprimeagen/harpoon',
  version = 'harpoon2',
  data = {
    deps = {
      'https://github.com/nvim-lua/plenary.nvim',
    },
    postload = function()
      local harpoon = require('harpoon')

      harpoon:setup()

      vim.keymap.set('n', '<C-p>', function()
        harpoon:list():prepend()
      end)
      vim.keymap.set('n', '<C-a>', function()
        harpoon:list():add()
      end)
      vim.keymap.set('n', '<C-e>', function()
        harpoon.ui:toggle_quick_menu(harpoon:list())
      end)

      vim.keymap.set('n', '<C-h>', function()
        harpoon:list():select(1)
      end)
      vim.keymap.set('n', '<C-j>', function()
        harpoon:list():select(2)
      end)
      vim.keymap.set('n', '<C-k>', function()
        harpoon:list():select(3)
      end)
      vim.keymap.set('n', '<C-l>', function()
        harpoon:list():select(4)
      end)

      vim.keymap.set('n', '<Leader><C-h>', function()
        harpoon:list():replace_at(1)
      end)
      vim.keymap.set('n', '<Leader><C-j>', function()
        harpoon:list():replace_at(2)
      end)
      vim.keymap.set('n', '<Leader><C-k>', function()
        harpoon:list():replace_at(3)
      end)
      vim.keymap.set('n', '<Leader><C-l>', function()
        harpoon:list():replace_at(4)
      end)
    end,
  },
}
