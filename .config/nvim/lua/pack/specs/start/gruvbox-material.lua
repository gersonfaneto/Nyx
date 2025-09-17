return {
  src = 'https://github.com/sainnhe/gruvbox-material',
  data = {
    init = function()
      vim.g.gruvbox_material_transparent_background = 0
      vim.g.gruvbox_material_foreground = 'mix'
      vim.g.gruvbox_material_background = 'medium'
      vim.g.gruvbox_material_ui_contrast = 'high'
      vim.g.gruvbox_material_float_style = 'bright'
      vim.g.gruvbox_material_statusline_style = 'mix'
      vim.g.gruvbox_material_cursor = 'auto'
    end,
  },
}
