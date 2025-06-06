-- Lazy-load builtin plugins

-- vscode-neovim
if vim.g.vscode then
  vim.fn['plugin#vscode#setup']()
  return
end

-- expandtab
vim.api.nvim_create_autocmd('InsertEnter', {
  once = true,
  group = vim.api.nvim_create_augroup('ExpandTabSetup', {}),
  callback = function()
    require('plugin.expandtab').setup()
  end,
})

-- jupytext
vim.api.nvim_create_autocmd('BufReadCmd', {
  once = true,
  pattern = '*.ipynb',
  group = vim.api.nvim_create_augroup('JupyTextSetup', {}),
  callback = function(info)
    require('plugin.jupytext').setup(info.buf)
  end,
})

-- lsp & diagnostic settings
vim.api.nvim_create_autocmd({ 'LspAttach', 'DiagnosticChanged' }, {
  once = true,
  desc = 'Apply lsp and diagnostic settings.',
  group = vim.api.nvim_create_augroup('LspDiagnosticSetup', {}),
  callback = function()
    require('plugin.lsp').setup()
  end,
})

-- readline
vim.api.nvim_create_autocmd({ 'CmdlineEnter', 'InsertEnter' }, {
  group = vim.api.nvim_create_augroup('ReadlineSetup', {}),
  once = true,
  callback = function()
    require('plugin.readline').setup()
  end,
})

-- winbar
vim.api.nvim_create_autocmd('FileType', {
  once = true,
  group = vim.api.nvim_create_augroup('WinBarSetup', {}),
  callback = function()
    if vim.g.loaded_winbar ~= nil then
      return
    end

    local winbar = require('plugin.winbar')
    local api = require('plugin.winbar.api')
    winbar.setup({ bar = { hover = false } })

    -- stylua: ignore start
    vim.keymap.set('n', '<Leader>;', api.pick, { desc = 'Pick symbols in winbar' })
    vim.keymap.set('n', '[;', api.goto_context_start, { desc = 'Go to start of current context' })
    vim.keymap.set('n', '];', api.select_next_context, { desc = 'Select next context' })
    -- stylua: ignore end
  end,
})

---Load ui elements e.g. tabline, statusline, statuscolumn
---@param name string
local function load_ui(name)
  local loaded_flag = 'loaded_' .. name
  if vim.g[loaded_flag] ~= nil then
    return
  end
  vim.g[loaded_flag] = true
  vim.opt[name] = string.format("%%!v:lua.require'plugin.%s'()", name)
end

load_ui('tabline')
load_ui('statusline')
load_ui('statuscolumn')

-- term
vim.api.nvim_create_autocmd('TermOpen', {
  group = vim.api.nvim_create_augroup('TermSetup', {}),
  callback = function(info)
    local term = require('plugin.term')

    term.setup()
    vim.keymap.set('n', '.', term.rerun, {
      buffer = info.buf,
      desc = 'Re-run terminal job',
    })
  end,
})

-- tmux
if vim.g.has_ui then
  vim.api.nvim_create_autocmd('UIEnter', {
    group = vim.api.nvim_create_augroup('TmuxSetup', {}),
    desc = 'Init tmux plugin.',
    once = true,
    callback = vim.schedule_wrap(function()
      require('plugin.tmux').setup()
    end),
  })
end

-- tabout
vim.api.nvim_create_autocmd({ 'InsertEnter', 'CmdlineEnter' }, {
  group = vim.api.nvim_create_augroup('TabOutSetup', {}),
  desc = 'Init tabout plugin.',
  once = true,
  callback = function()
    require('plugin.tabout').setup()
  end,
})

-- z
do
  local opts = {
    group = vim.api.nvim_create_augroup('ZSetup', {}),
    desc = 'Init z plugin.',
    once = true,
    callback = function()
      if vim.g.loaded_z then
        return
      end

      local z = require('plugin.z')
      z.setup()
      vim.keymap.set('n', '<Leader>z', z.select, {
        desc = 'Open a directory from z',
      })
    end,
  }

  vim.api.nvim_create_autocmd({ 'CmdlineEnter', 'DirChanged' }, opts)
  vim.api.nvim_create_autocmd(
    'UIEnter',
    vim.tbl_deep_extend('force', opts, {
      callback = vim.schedule_wrap(opts.callback),
    })
  )
  vim.api.nvim_create_autocmd(
    'CmdUndefined',
    vim.tbl_deep_extend('force', opts, { pattern = 'Z*' })
  )
end

-- addasync
vim.api.nvim_create_autocmd('InsertEnter', {
  group = vim.api.nvim_create_augroup('AddAsyncSetup', {}),
  desc = 'Init addasync plugin.',
  callback = function()
    if require('utils.ts').is_active() then
      require('plugin.addasync').setup()
      return true
    end
  end,
})
