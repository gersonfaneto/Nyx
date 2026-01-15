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

      -- stylua: ignore start
      vim.keymap.set({ 'n' }, '<C-p>', function() harpoon:list():prepend() end, { desc = 'Harpoon :: Prepend with current' })
      vim.keymap.set({ 'n' }, '<C-a>', function() harpoon:list():add() end, { desc = 'Harpoon :: Append with current' })
      vim.keymap.set({ 'n' }, '<C-e>', function() harpoon.ui:toggle_quick_menu(harpoon:list()) end, { desc = 'Harpoon :: Show list' })

      vim.keymap.set({ 'n' }, '<C-h>', function() harpoon:list():select(1) end, { desc = 'Harpoon :: Visit file (1)' })
      vim.keymap.set({ 'n' }, '<C-j>', function() harpoon:list():select(2) end, { desc = 'Harpoon :: Visit file (2)' })
      vim.keymap.set({ 'n' }, '<C-k>', function() harpoon:list():select(3) end, { desc = 'Harpoon :: Visit file (3)' })
      vim.keymap.set({ 'n' }, '<C-l>', function() harpoon:list():select(4) end, { desc = 'Harpoon :: Visit file (4)' })

      vim.keymap.set({ 'n' }, '<Leader><C-h>', function() harpoon:list():replace_at(1) end, { desc = 'Harpoon :: Replace file (1) with current' })
      vim.keymap.set({ 'n' }, '<Leader><C-j>', function() harpoon:list():replace_at(2) end, { desc = 'Harpoon :: Replace file (2) with current' })
      vim.keymap.set({ 'n' }, '<Leader><C-k>', function() harpoon:list():replace_at(3) end, { desc = 'Harpoon :: Replace file (3) with current' })
      vim.keymap.set({ 'n' }, '<Leader><C-l>', function() harpoon:list():replace_at(4) end, { desc = 'Harpoon :: Replace file (4) with current' })
      -- stylua: ignore end
    end,
  },
}
