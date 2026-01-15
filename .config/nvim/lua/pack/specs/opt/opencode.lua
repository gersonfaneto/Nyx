---@type pack.spec
return {
  src = 'https://github.com/NickvanDyke/opencode.nvim',
  data = {
    deps = {
      src = 'https://github.com/folke/snacks.nvim',
    },
    postload = function()
      if vim.fn.executable('opencode') == 0 then
        vim.notify(
          '[Opencode.nvim] command `opencode` not found',
          vim.log.levels.ERROR
        )
        return
      end

      local opencode = require('opencode')

      vim.g.opencode_opts = {}

      vim.o.autoread = true

      -- stylua: ignore start
      vim.keymap.set({ 'n', 'x' }, '<Leader>ca', function() opencode.ask('@this: ', { submit = true }) end, { desc = 'Ask opencode' })
      vim.keymap.set({ 'n', 'x' }, '<Leader>cx', function() opencode.select() end, { desc = 'Execute opencode action…' })
      vim.keymap.set({ 'n', 't' }, '<Leader>c.', function() opencode.toggle() end, { desc = 'Toggle opencode' })

      vim.keymap.set({ 'n', 'x' }, '<Leader>cor', function() return opencode.operator('@this ') end, { expr = true, desc = 'Add range to opencode' })
      vim.keymap.set({ 'n' }, '<Leader>col', function() return opencode.operator('@this ') .. '_' end, { expr = true, desc = 'Add line to opencode' })

      vim.keymap.set({ 'n' }, '<S-C-u>', function() opencode.command('session.half.page.up') end, { desc = 'Opencode half page up' })
      vim.keymap.set({ 'n' }, '<S-C-d>', function() opencode.command('session.half.page.down') end, { desc = 'Opencode half page down' })
      -- stylua: ignore end
    end,
  },
}
