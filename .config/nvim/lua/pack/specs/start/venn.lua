---@type pack.spec
return {
  src = 'https://github.com/jbyuki/venn.nvim',
  data = {
    keys = {
      {
        lhs = '<Leader>v',
        mode = { 'n', 'x' },
        opts = { noremap = true, desc = 'Enable [V]enn diagramming mode' },
      },
    },
    postload = function()
      vim.keymap.set({ 'n', 'x' }, '<Leader>v', function()
        local is_enabled = vim.inspect(vim.b['venn.enabled'])

        if is_enabled == 'nil' then
          vim.b['venn.enabled'] = true

          vim.wo.virtualedit = 'all'

          vim.keymap.set('n', 'J', '<C-v>j:VBox<CR>', { buffer = true })
          vim.keymap.set('n', 'K', '<C-v>k:VBox<CR>', { buffer = true })
          vim.keymap.set('n', 'L', '<C-v>l:VBox<CR>', { buffer = true })
          vim.keymap.set('n', 'H', '<C-v>h:VBox<CR>', { buffer = true })

          vim.keymap.set('v', 'f', ':VBox<CR>', { buffer = true })
        else
          vim.b['venn.enabled'] = nil

          vim.wo.virtualedit = ''

          vim.keymap.del('n', 'J', { buffer = true })
          vim.keymap.del('n', 'K', { buffer = true })
          vim.keymap.del('n', 'L', { buffer = true })
          vim.keymap.del('n', 'H', { buffer = true })

          vim.keymap.del('v', 'f', { buffer = true })
        end
      end)
    end,
  },
}
