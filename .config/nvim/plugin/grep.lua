if vim.fn.executable('rg') == 0 then
  return
end

vim.o.grepprg = 'rg --vimgrep --smart-case --hidden'
vim.o.grepformat = '%f:%l:%c:%m,%f:%l:%m'

-- stylua: ignore start
vim.keymap.set('n', '\\', ":silent grep! '' | cwindow | cfirst" .. string.rep('<Left>', 20), { desc = 'Grep' })
-- stylua: ignore end
