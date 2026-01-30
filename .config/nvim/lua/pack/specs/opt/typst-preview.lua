---@type pack.spec
return {
  src = 'https://github.com/chomosuke/typst-preview.nvim',
  version = vim.version.range('1.*'),
  data = {
    events = {
      event = 'FileType',
      pattern = 'typst',
    },
    postload = function()
      require('typst-preview').setup({
        open_cmd = 'firefox %s --class typst-preview 2>/dev/null',
        dependencies_bin = {
          ['tinymist'] = 'tinymist',
          ['websocat'] = 'websocat',
        },
      })

      -- stylua: ignore start
      vim.keymap.set({'n'}, '<LocalLeader>p', '<CMD>TypstPreview<CR>', { desc = 'Start Preview' })
      vim.keymap.set({'n'}, '<LocalLeader>P', '<CMD>TypstPreviewStop<CR>', { desc = 'Stop Preview' })
      -- stylua: ignore end
    end,
  },
}
