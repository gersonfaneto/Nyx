---@type pack.spec
return {
  src = 'https://github.com/folke/snacks.nvim',
  data = {
    postload = function()
      local snacks = require('snacks')
      snacks.setup({
        input = { enabled = true },
        picker = { enabled = true },
        explorer = { enabled = true, replace_netrw = false },
        terminal = { enabled = true },
      })

      -- stylua: ignore start
      vim.keymap.set({ 'n' }, '<C-p>', Snacks.picker.explorer, { desc = 'Snacks :: Explorer' }) -- luacheck: ignore 113
      -- stylua: ignore end
    end,
  },
}
