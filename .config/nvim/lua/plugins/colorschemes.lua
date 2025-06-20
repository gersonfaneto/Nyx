return {
  {
    'sainnhe/everforest',
    init = function()
      vim.g.everforest_transparent_background = 0
      vim.g.everforest_background = 'hard'
      vim.g.everforest_ui_contrast = 'high'
      vim.g.everforest_float_style = 'bright'
      vim.g.everforest_cursor = 'auto'
      vim.g.everforest_better_performance = 1
    end,
  },
  {
    'sainnhe/gruvbox-material',
    init = function()
      vim.g.gruvbox_material_transparent_background = 0
      vim.g.gruvbox_material_foreground = 'mix'
      vim.g.gruvbox_material_background = 'hard'
      vim.g.gruvbox_material_ui_contrast = 'high'
      vim.g.gruvbox_material_float_style = 'bright'
      vim.g.gruvbox_material_statusline_style = 'material'
      vim.g.gruvbox_material_cursor = 'auto'
      vim.g.gruvbox_material_better_performance = 1
    end,
  },
}
