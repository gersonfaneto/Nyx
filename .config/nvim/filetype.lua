vim.api.nvim_create_autocmd({ 'BufReadPre', 'FileType' }, {
  once = true,
  callback = function()
    vim.filetype.add({
      extension = {
        asm = 'fasm',
        v = 'verilog',
      },
    })
  end,
})
