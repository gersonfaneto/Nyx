--@type pack.spec
return {
  src = 'https://github.com/sainnhe/gruvbox-material',
  data = {
    preload = function()
      vim.g.gruvbox_material_transparent_background = 0
      vim.g.gruvbox_material_foreground = 'original'
      vim.g.gruvbox_material_background = 'hard'
      vim.g.gruvbox_material_ui_contrast = 'high'
      vim.g.gruvbox_material_float_style = 'bright'
      vim.g.gruvbox_material_statusline_style = 'original'
      vim.g.gruvbox_material_cursor = 'auto'
    end,
    postload = function()
      vim.opt.background = 'dark'
      vim.cmd('colorscheme gruvbox-material')
    end,
  },
}
