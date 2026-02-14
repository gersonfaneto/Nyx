---@type minimal.pack.spec
return {
  src = 'https://github.com/julienvincent/nvim-paredit',
  data = {
    events = {
      event = 'FileType',
      pattern = 'clojure',
    },
    postload = function()
      local paredit = require('nvim-paredit')

      paredit.setup({
        filetypes = { 'clojure' },
      })

      vim.keymap.set({ 'n' }, '<LocalLeader>w', function()
        return paredit.cursor.place_cursor(
          paredit.wrap.wrap_element_under_cursor('( ', ')'),
          { placement = 'inner_start', mode = 'insert' }
        )
      end, { desc = 'Wrap element insert head' })

      vim.keymap.set({ 'n' }, '<LocalLeader>W', function()
        return paredit.cursor.place_cursor(
          paredit.wrap.wrap_element_under_cursor('(', ' )'),
          { placement = 'inner_end', mode = 'insert' }
        )
      end, { desc = 'Wrap element insert tail' })

      vim.keymap.set({ 'n' }, '<LocalLeader>i', function()
        return paredit.cursor.place_cursor(
          paredit.wrap.wrap_enclosing_form_under_cursor('( ', ')'),
          { placement = 'inner_start', mode = 'insert' }
        )
      end, { desc = 'Wrap form insert head' })

      vim.keymap.set({ 'n' }, '<LocalLeader>I', function()
        return paredit.cursor.place_cursor(
          paredit.wrap.wrap_enclosing_form_under_cursor('(', ' )'),
          { placement = 'inner_end', mode = 'insert' }
        )
      end, { desc = 'Wrap form insert tail' })
    end,
  },
}
