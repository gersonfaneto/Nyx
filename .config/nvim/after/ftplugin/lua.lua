-- stylua: ignore start
vim.keymap.set('n', '<leader>xf', '<CMD>source %<CR>', { desc = 'Source current file' })
vim.keymap.set({ 'n', 'x' }, '<Leader>xe', ':.lua<CR>', { desc = 'Source current selection' })
-- stylua: ignore end

