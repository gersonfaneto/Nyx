return {
  {
    'nvim-treesitter/nvim-treesitter',
    build = function()
      local ts_install_ok, ts_install =
        pcall(require, 'nvim-treesitter.install')
      if ts_install_ok then
        ts_install.update()
      end
    end,
    cmd = {
      'TSInstall',
      'TSInstallSync',
      'TSInstallInfo',
      'TSUninstall',
      'TSUpdate',
      'TSUpdateSync',
      'TSBufEnable',
      'TSBufToggle',
      'TSEnable',
      'TSToggle',
      'TSModuleInfo',
      'TSEditQuery',
      'TSEditQueryUserAfter',
    },
    event = 'FileType',
    -- Some parsers, e.g. cuda parsers can slow down file reading speed
    -- if loaded on `FileType` event, so defer setting up nvim-treesitter
    -- to first read the file then enable treesitter afterwards
    config = vim.schedule_wrap(function()
      require('configs.nvim-treesitter')
    end),
  },

  {
    'nvim-treesitter/nvim-treesitter-textobjects',
    dependencies = 'nvim-treesitter/nvim-treesitter',
    event = 'FileType',
    -- Plugins that require nvim-treesitter and loaded on `FileType` must be scheduled
    -- as well to avoid loading nvim-treesitter early
    config = vim.schedule_wrap(function()
      require('configs.nvim-treesitter-textobjects')
    end),
  },

  {
    'RRethy/nvim-treesitter-endwise',
    dependencies = 'nvim-treesitter/nvim-treesitter',
    event = 'InsertEnter',
    config = function()
      -- Manually trigger `FileType` event to make nvim-treesitter-endwise
      -- attach to current file when loaded
      vim.api.nvim_exec_autocmds('FileType', {})
    end,
  },

  {
    'tronikelis/ts-autotag.nvim',
    event = 'InsertEnter',
    dependencies = 'nvim-treesitter/nvim-treesitter',
    config = true,
  },

  {
    'Wansmer/treesj',
    cmd = { 'TSJToggle', 'TSJSplit', 'TSJJoin' },
    keys = {
      { 'gsj', desc = 'Join current treesitter node' },
      { 'gss', desc = 'Split current treesitter node' },
      { 'gsS', desc = 'Split current treesitter node recursively' },
    },
    dependencies = 'nvim-treesitter/nvim-treesitter',
    config = function()
      require('configs.treesj')
    end,
  },

  {
    'Eandrju/cellular-automaton.nvim',
    cmd = 'CellularAutomaton',
    dependencies = 'nvim-treesitter/nvim-treesitter',
  },
}
