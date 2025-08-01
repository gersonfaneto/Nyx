if vim.g.loaded_intro ~= nil then
  return
end

vim.g.loaded_intro = true

---Check if the intro message is disabled by user
---@return boolean
local function intro_disabled()
  return vim.go.shortmess:find('I', 1, true) ~= nil
end

---Disable builtin intro message
---@return nil
local function disable_intro()
  local eventignore = vim.go.eventignore
  vim.opt.eventignore:append('OptionSet')
  vim.opt.shortmess:append('I')
  vim.go.eventignore = eventignore
end

---@return boolean
local function should_show_intro()
  return vim.g.has_ui and vim.fn.argc(-1) == 0 and not intro_disabled()
end

if not should_show_intro() then
  disable_intro()
  return
end

-- Set eventignore to avoid triggering plugin lazy-loading handlers
local eventignore = vim.go.eventignore
vim.go.eventignore = 'all'

local buf ---@type integer?
local win ---@type integer?

---Wipe out the intro message buffer
---@return nil
local function clear_intro()
  if buf and vim.api.nvim_buf_is_valid(buf) then
    vim.api.nvim_buf_delete(buf, { force = true })
  end
end

local groupid = vim.api.nvim_create_augroup('NvimIntro', {})
vim.api.nvim_create_autocmd('UIEnter', {
  group = groupid,
  once = true,
  desc = 'Show the intro message on entering the UI.',
  callback = function()
    ---@class intro_chunk_t
    ---@field text string
    ---@field hl string
    ---@field len integer? byte-indexed text length
    ---@field width integer? display width of text

    ---@class intro_line_t
    ---@field chunks intro_chunk_t[]
    ---@field text string?
    ---@field width integer?
    ---@field offset integer?

    ---Lines of text and highlight groups to display as intro message
    ---@type intro_line_t[]
    local lines = {
      {
        chunks = {
          {
            text = string.format(
              '%s',
              vim.go.termguicolors and 'M I N I M Λ L' or 'M I N I M A L'
            ),
            hl = 'Normal',
          },
        },
      },
      {
        chunks = {
          { text = 'by @gersonfaneto', hl = 'comment' },
        },
      },
      { chunks = {} },
      {
        chunks = not math.randomseed() and math.random() < 0.5 and {
          {
            text = vim.fn.keytrans(vim.g.mapleader or '\\'),
            hl = 'comment',
          },
          { text = ' to start', hl = 'Normal' },
        } or {
          { text = ':h', hl = 'comment' },
          { text = ' for help', hl = 'Normal' },
        },
      },
    }

    ---Window configuration for the intro message floating window
    ---@type vim.api.keyset.win_config
    local win_config = {
      width = 0,
      height = #lines,
      relative = 'editor',
      style = 'minimal',
      focusable = false,
      noautocmd = true,
      zindex = 1,
    }

    ---Calculate the width, offset, concatenated text, etc.
    for _, line in ipairs(lines) do
      line.text = ''
      line.width = 0
      for _, chunk in ipairs(line.chunks) do
        chunk.len = #chunk.text
        chunk.width = vim.fn.strdisplaywidth(chunk.text)
        line.text = line.text .. chunk.text
        line.width = line.width + chunk.width
      end
      if line.width > win_config.width then
        win_config.width = line.width
      end
    end

    for _, line in ipairs(lines) do
      line.offset = math.floor((win_config.width - line.width) / 2)
    end

    -- Decide the row and col offset of the floating window,
    -- return if no enough space
    win_config.row =
      math.floor((vim.go.lines - vim.go.ch - win_config.height) / 2)
    win_config.col = math.floor((vim.go.columns - win_config.width) / 2)
    if win_config.row < 2 or win_config.col < 2 then
      disable_intro()
      -- Restore &eventignore before exit
      vim.go.eventignore = eventignore
      return
    end

    -- Create the scratch buffer to display the intro message
    buf = vim.api.nvim_create_buf(false, true)
    vim.bo[buf].bufhidden = 'wipe'
    vim.bo[buf].buftype = 'nofile'
    vim.bo[buf].swapfile = false
    vim.bo[buf].modifiable = true -- fix error when used with `nvim -M`
    vim.api.nvim_buf_set_lines(
      buf,
      0,
      -1,
      false,
      vim.tbl_map(function(line)
        return string.rep(' ', line.offset) .. line.text
      end, lines)
    )

    -- Apply highlight groups
    local ns = vim.api.nvim_create_namespace('NvimIntro')
    for linenr, line in ipairs(lines) do
      local chunk_offset = line.offset
      for _, chunk in ipairs(line.chunks) do
        vim.hl.range(
          buf,
          ns,
          chunk.hl,
          { linenr - 1, chunk_offset },
          { linenr - 1, chunk_offset + chunk.len },
          {}
        )
        chunk_offset = chunk_offset + chunk.len
      end
    end

    if not should_show_intro() then
      disable_intro()
      clear_intro()
      return
    end

    disable_intro()

    -- Intro message buffer can be deleted by user actions before the UIEnter
    -- event, e.g. when nvim is launched using `nvim +split`
    if not vim.api.nvim_buf_is_valid(buf) then
      return
    end

    win = vim.api.nvim_open_win(buf, false, win_config)

    -- Using `vim.w[win]` will change global value and affect other windows
    vim.api.nvim_win_call(win, function()
      vim.opt_local.winhl = 'NormalFloat:Normal,Search:,Incsearch:'
      vim.opt_local.spell = false
    end)
  end,
})

vim.api.nvim_create_autocmd({
  'BufAdd',
  'BufModifiedSet',
  'BufReadPre',
  'CursorMoved',
  'InsertEnter',
  'SessionLoadPost',
  'StdinReadPre',
  'TermEnter',
  'TermOpen',
  'TextChanged',
  'VimResized',
  'WinEnter',
}, {
  once = true,
  group = groupid,
  desc = 'Clear the intro on user action.',
  callback = function()
    disable_intro()
    clear_intro()
  end,
})

vim.api.nvim_create_autocmd('OptionSet', {
  group = groupid,
  pattern = 'shortmess',
  desc = 'Clear the intro if intro message is disabled by user.',
  callback = function()
    if intro_disabled() then
      clear_intro()
      return true
    end
  end,
})

-- Restore &eventignore before exit
vim.go.eventignore = eventignore
