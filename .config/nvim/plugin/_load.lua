-- Lazy-load builtin plugins

local load = require('utils.load')

-- expandtab
load.on_events('InsertEnter', 'plugin.expandtab', function()
  require('plugin.expandtab').setup()
end)

-- lsp & diagnostic commands
load.on_events(
  { 'Syntax', 'FileType', 'LspAttach', 'DiagnosticChanged' },
  'plugin.lsp-commands',
  function()
    require('plugin.lsp-commands').setup()
  end
)

-- readline
load.on_events({ 'CmdlineEnter', 'InsertEnter' }, 'plugin.readline', function()
  require('plugin.readline').setup()
end)

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
load.on_events('TermOpen', 'plugin.term', function(args)
  local term = require('plugin.term')
  term.setup()
  vim.keymap.set('n', '.', term.rerun, {
    buffer = args.buf,
    desc = 'Re-run terminal job',
  })
end)

-- tmux
if vim.g.has_ui then
  load.on_events(
    { event = 'UIEnter' },
    'plugin.tmux',
    vim.schedule_wrap(function()
      require('plugin.tmux').setup()
    end)
  )
end

-- tabout
load.on_events('InsertEnter', 'plugin.tabout', function()
  require('plugin.tabout').setup()
end)

-- z
if vim.g.loaded_z == nil then
  vim.keymap.set('n', '<Leader>z', function()
    require('plugin.z').select()
  end, { desc = 'Open a directory from z' })

  local function setup()
    require('plugin.z').setup()
  end

  load.on_events('UIEnter', 'plugin.z', vim.schedule_wrap(setup))
  load.on_events('DirChanged', 'plugin.z', setup)
  load.on_cmds({ 'Z', 'ZSelect' }, 'plugin.z', setup)
end

-- addasync
load.on_events('InsertEnter', 'plugin.addaync', function()
  require('plugin.addasync').setup()
end)

-- session
if vim.g.loaded_session == nil then
  vim.keymap.set('n', '<Leader>ww', function()
    require('plugin.session').select(true)
  end, { desc = 'Load session (workspace) interactively' })

  vim.keymap.set('n', '<Leader>wW', function()
    require('plugin.session').load(nil, true)
  end, { desc = 'Load session (workspace) for cwd' })

  vim.keymap.set('n', '<Leader>ws', function()
    require('plugin.session').save(nil, true)
  end, { desc = 'Save session (workspace) at cwd' })

  vim.keymap.set('n', '<Leader>wr', function()
    require('plugin.session').remove(nil)
  end, { desc = 'Remove session (workspace) at cwd' })

  local function setup()
    require('plugin.session').setup({
      autoload = { enabled = false },
      autoremove = { enabled = false },
    })
  end

  load.on_events('BufRead', 'plugin.session', setup)
  load.on_cmds({
    'SessionLoad',
    'SessionSave',
    'SessionRemove',
    'SessionSelect',
    'Mkssession',
  }, 'plugin.session', setup)
end
