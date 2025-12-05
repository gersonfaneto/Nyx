vim.keymap.set({ 'n' }, '<CR>', function()
  -- save cursor position
  local cursor = vim.api.nvim_win_get_cursor(0)
  local content = vim.api.nvim_get_current_line()
  local res = vim.fn.match(content, '\\[ \\]')
  if res == -1 then
    vim.fn.execute('.s/\\[[x~]\\]/[ ]')
  else
    vim.fn.execute('.s/\\[ \\]/[x]')
  end
  -- restore cursor position
  vim.api.nvim_win_set_cursor(0, cursor)
  -- remove highlights
  vim.cmd.nohlsearch()
end, { buffer = 0, silent = true, desc = 'Toggle checkbox' })
