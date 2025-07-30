-- stylua: ignore start
vim.keymap.set('n', '<Leader>x', '<Cmd>.lua<CR>', { desc = 'Execute the current line' })
vim.keymap.set( 'n', '<Leader><Leader>x', '<Cmd>source %<CR>', { desc = 'Execute the current file' })
-- stylua: ignore end
