local M = {}
local term_utils = require('utils.term')

---@param buf? integer terminal buffer id
---@return boolean
local function validate_term_buf(buf)
  buf = buf or 0
  if vim.api.nvim_buf_is_valid(buf) and vim.bo[buf].bt == 'terminal' then
    return true
  end
  vim.notify(
    string.format('[term] buffer %d is not a terminal buffer', buf),
    vim.log.levels.WARN
  )
  return false
end

---@param cmd? string new command
---@param buf? integer terminal buffer id
function M.set_cmd(cmd, buf)
  buf = buf or 0
  if not validate_term_buf(buf) then
    return
  end
  ---@cast cmd string
  if not cmd or cmd == '' then
    return
  end
  if vim.fn.executable(cmd) == 0 then
    vim.notify(
      string.format('[term] command `%s` is not executable', cmd),
      vim.log.levels.WARN
    )
    return
  end
  vim.cmd.file(
    vim.fn.fnameescape(
      term_utils.compose_name(vim.api.nvim_buf_get_name(buf), { cmd = cmd })
    )
  )
end

---@param path? string
---@param buf? integer terminal buffer id
function M.set_path(path, buf)
  buf = buf or 0
  if not validate_term_buf(buf) then
    return
  end
  ---@cast path string
  if not path or path == '' then
    path = vim.fn.getcwd(0)
  end
  if not vim.fn.isdirectory(path) then
    vim.notify(string.format("[term] path '%s' is not a directory", path))
    return
  end
  vim.cmd.file(
    vim.fn.fnameescape(
      term_utils.compose_name(vim.api.nvim_buf_get_name(buf), { path = path })
    )
  )
end

---@param buf integer? terminal buffer handler
function M.rerun(buf)
  buf = buf or 0
  if not validate_term_buf(buf) then
    return
  end
  vim.cmd.edit(
    vim.fn.fnameescape(
      term_utils.compose_name(vim.api.nvim_buf_get_name(buf), { pid = '' })
    )
  )
end

---@param name? string
---@param buf? integer
function M.rename(name, buf)
  buf = buf or 0
  if not validate_term_buf(buf) then
    return
  end
  if not name then
    return
  end
  vim.cmd.file(
    vim.fn.fnameescape(
      term_utils.compose_name(vim.api.nvim_buf_get_name(0), { name = name })
    )
  )
end

---Initial setup for a terminal buffer
---@param buf integer? terminal buffer handler
---@return nil
function M.term_init(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  if not vim.api.nvim_buf_is_valid(buf) or vim.bo[buf].bt ~= 'terminal' then
    return
  end

  for _, win in ipairs(vim.fn.win_findbuf(buf)) do
    vim.wo[win][0].nu = false
    vim.wo[win][0].rnu = false
    vim.wo[win][0].spell = false
    vim.wo[win][0].statuscolumn = ''
    vim.wo[win][0].signcolumn = 'no'
  end

  -- Start with insert mode in new terminals
  -- Use `vim.schedule()` to avoid ending with insert mode in a normal buffer
  -- after loading a session with terminal buffers
  vim.schedule(function()
    if vim.api.nvim_get_current_buf() == buf then
      vim.cmd.startinsert()
    end
  end)

  -- Create commands to rename terminals
  vim.api.nvim_buf_create_user_command(buf, 'TermRename', function(args)
    M.rename(args.args)
  end, {
    nargs = '?',
    desc = 'Rename current terminal',
    complete = function()
      local term_names = {}

      for _, b in ipairs(vim.api.nvim_list_bufs()) do
        if vim.bo[b].bt ~= 'terminal' then
          goto continue
        end
        local _, _, _, name =
          term_utils.parse_name(vim.api.nvim_buf_get_name(b))
        if name == '' then
          goto continue
        end
        term_names[name] = true
        ::continue::
      end

      local compl = {}
      local _, _, _, curr_name =
        term_utils.parse_name(vim.api.nvim_buf_get_name(0))
      for name, _ in pairs(term_names) do
        if name == curr_name then
          table.insert(compl, 1, name)
        else
          table.insert(compl, name)
        end
      end

      return compl
    end,
  })

  vim.api.nvim_buf_create_user_command(buf, 'TermSetCmd', function(args)
    M.set_cmd(args.args)
  end, {
    nargs = '?',
    desc = 'Set cmd for current terminal',
    complete = 'shellcmdline',
  })

  vim.api.nvim_buf_create_user_command(buf, 'TermSetPath', function(args)
    M.set_path(args.args)
  end, {
    nargs = '?',
    desc = 'Set path for current terminal',
    complete = 'dir',
  })

  vim.api.nvim_buf_create_user_command(buf, 'TermRerun', function(args)
    M.rerun(tonumber(args.args))
  end, {
    nargs = '?',
    desc = 'Re-run terminal command',
    complete = function()
      local terms = {}

      for _, b in ipairs(vim.api.nvim_list_bufs()) do
        if vim.bo[b].bt == 'terminal' then
          table.insert(
            terms,
            string.format('%d (%s)', b, vim.api.nvim_buf_get_name(b))
          )
        end
      end

      return terms
    end,
  })
end

---Plugin initialize function
---@return nil
function M.setup()
  if vim.g.loaded_term_plugin ~= nil then
    return
  end
  vim.g.loaded_term_plugin = true

  -- stylua: ignore start
  vim.keymap.set('t', '<C-6>', [[v:lua.require'utils.term'.running_tui() ? "<C-6>" : "<Cmd>b#<CR>"]], { expr = true, replace_keycodes = false, desc = 'Go to alternate buffer' })
  vim.keymap.set('t', '<C-^>', [[v:lua.require'utils.term'.running_tui() ? "<C-^>" : "<Cmd>b#<CR>"]], { expr = true, replace_keycodes = false, desc = 'Go to alternate buffer' })
  vim.keymap.set('t', '<Esc>', [[v:lua.require'utils.term'.running_tui() ? "<Esc>" : "<Cmd>stopi<CR>"]], { expr = true, replace_keycodes = false, desc = 'Stop terminal mode' })
  vim.keymap.set('t', '<M-v>', [[v:lua.require'utils.term'.running_tui() ? "<M-v>" : "<Cmd>wincmd v<CR>"]], { expr = true, replace_keycodes = false, desc = 'Split window vertically' })
  vim.keymap.set('t', '<M-s>', [[v:lua.require'utils.term'.running_tui() ? "<M-s>" : "<Cmd>wincmd s<CR>"]], { expr = true, replace_keycodes = false, desc = 'Split window horizontally' })
  vim.keymap.set('t', '<M-W>', [[v:lua.require'utils.term'.running_tui() ? "<M-W>" : "<Cmd>wincmd W<CR>"]], { expr = true, replace_keycodes = false, desc = 'Go to the previous window' })
  vim.keymap.set('t', '<M-H>', [[v:lua.require'utils.term'.running_tui() ? "<M-H>" : "<Cmd>wincmd H<CR>"]], { expr = true, replace_keycodes = false, desc = 'Move window to far left' })
  vim.keymap.set('t', '<M-J>', [[v:lua.require'utils.term'.running_tui() ? "<M-J>" : "<Cmd>wincmd J<CR>"]], { expr = true, replace_keycodes = false, desc = 'Move window to very bottom' })
  vim.keymap.set('t', '<M-K>', [[v:lua.require'utils.term'.running_tui() ? "<M-K>" : "<Cmd>wincmd K<CR>"]], { expr = true, replace_keycodes = false, desc = 'Move window to very top' })
  vim.keymap.set('t', '<M-L>', [[v:lua.require'utils.term'.running_tui() ? "<M-L>" : "<Cmd>wincmd L<CR>"]], { expr = true, replace_keycodes = false, desc = 'Move window to far right' })
  vim.keymap.set('t', '<M-r>', [[v:lua.require'utils.term'.running_tui() ? "<M-r>" : "<Cmd>wincmd r<CR>"]], { expr = true, replace_keycodes = false, desc = 'Rotate windows downwards' })
  vim.keymap.set('t', '<M-R>', [[v:lua.require'utils.term'.running_tui() ? "<M-R>" : "<Cmd>wincmd R<CR>"]], { expr = true, replace_keycodes = false, desc = 'Rotate windows upwards' })
  vim.keymap.set('t', '<M-x>', [[v:lua.require'utils.term'.running_tui() ? "<M-x>" : "<Cmd>wincmd x<CR>"]], { expr = true, replace_keycodes = false, desc = 'Exchange window with next one' })
  vim.keymap.set('t', '<M-p>', [[v:lua.require'utils.term'.running_tui() ? "<M-p>" : "<Cmd>wincmd p<CR>"]], { expr = true, replace_keycodes = false, desc = 'Go to previous window' })
  vim.keymap.set('t', '<M-c>', [[v:lua.require'utils.term'.running_tui() ? "<M-c>" : "<Cmd>wincmd c<CR>"]], { expr = true, replace_keycodes = false, desc = 'Close window' })
  vim.keymap.set('t', '<M-q>', [[v:lua.require'utils.term'.running_tui() ? "<M-q>" : "<Cmd>wincmd q<CR>"]], { expr = true, replace_keycodes = false, desc = 'Quit window' })
  vim.keymap.set('t', '<M-o>', [[v:lua.require'utils.term'.running_tui() ? "<M-o>" : "<Cmd>wincmd o<CR>"]], { expr = true, replace_keycodes = false, desc = 'Make window only one' })
  vim.keymap.set('t', '<M-w>', [[v:lua.require'utils.term'.running_tui() ? "<M-w>" : "<Cmd>wincmd w<CR>"]], { expr = true, replace_keycodes = false, desc = 'Go to next window' })
  vim.keymap.set('t', '<M-h>', [[v:lua.require'utils.term'.running_tui() ? "<M-h>" : "<Cmd>wincmd h<CR>"]], { expr = true, replace_keycodes = false, desc = 'Go to left window' })
  vim.keymap.set('t', '<M-j>', [[v:lua.require'utils.term'.running_tui() ? "<M-j>" : "<Cmd>wincmd j<CR>"]], { expr = true, replace_keycodes = false, desc = 'Go to window below' })
  vim.keymap.set('t', '<M-k>', [[v:lua.require'utils.term'.running_tui() ? "<M-k>" : "<Cmd>wincmd k<CR>"]], { expr = true, replace_keycodes = false, desc = 'Go to window above' })
  vim.keymap.set('t', '<M-l>', [[v:lua.require'utils.term'.running_tui() ? "<M-l>" : "<Cmd>wincmd l<CR>"]], { expr = true, replace_keycodes = false, desc = 'Go to right window' })
  vim.keymap.set('t', '<M-=>', [[v:lua.require'utils.term'.running_tui() ? "<M-=>" : "<Cmd>wincmd =<CR>"]], { expr = true, replace_keycodes = false, desc = 'Make windows equal size' })
  vim.keymap.set('t', '<M-_>', [[v:lua.require'utils.term'.running_tui() ? "<M-_>" : "<Cmd>wincmd _<CR>"]], { expr = true, replace_keycodes = false, desc = 'Set window height to highest possible' })
  vim.keymap.set('t', '<M-|>', [[v:lua.require'utils.term'.running_tui() ? "<M-|>" : "<Cmd>wincmd |<CR>"]], { expr = true, replace_keycodes = false, desc = 'Set window width to widest possible' })
  vim.keymap.set('t', '<M-+>', [[v:lua.require'utils.term'.running_tui() ? "<M-+>" : "<Cmd>wincmd 2+<CR>"]], { expr = true, replace_keycodes = false, desc = 'Increase window height' })
  vim.keymap.set('t', '<M-->', [[v:lua.require'utils.term'.running_tui() ? "<M-->" : "<Cmd>wincmd 2-<CR>"]], { expr = true, replace_keycodes = false, desc = 'Decrease window height' })
  vim.keymap.set('t', '<M->>', [[v:lua.require'utils.term'.running_tui() ? "<M->>" : "<Cmd>wincmd 4" . (winnr() == winnr("l") ? "<" : ">") . "<CR>"]], { expr = true, desc = 'Resize window right' })
  vim.keymap.set('t', '<M-<>', [[v:lua.require'utils.term'.running_tui() ? "<M-<>" : "<Cmd>wincmd 4" . (winnr() == winnr("l") ? ">" : "<") . "<CR>"]], { expr = true, desc = 'Resize window left' })
  vim.keymap.set('t', '<M-.>', [[v:lua.require'utils.term'.running_tui() ? "<M-.>" : "<Cmd>wincmd 4" . (winnr() == winnr("l") ? "<" : ">") . "<CR>"]], { expr = true, desc = 'Resize window right' })
  vim.keymap.set('t', '<M-,>', [[v:lua.require'utils.term'.running_tui() ? "<M-,>" : "<Cmd>wincmd 4" . (winnr() == winnr("l") ? ">" : "<") . "<CR>"]], { expr = true, desc = 'Resize window left' })
  vim.keymap.set('t', '<M-Left>', [[v:lua.require'utils.term'.running_tui() ? "<M-Left>" : "<Cmd>wincmd h<CR>"]], { expr = true, replace_keycodes = false, desc = 'Go to left window' })
  vim.keymap.set('t', '<M-Down>', [[v:lua.require'utils.term'.running_tui() ? "<M-Down>" : "<Cmd>wincmd j<CR>"]], { expr = true, replace_keycodes = false, desc = 'Go to window below' })
  vim.keymap.set('t', '<M-Up>', [[v:lua.require'utils.term'.running_tui() ? "<M-Up>" : "<Cmd>wincmd k<CR>"]], { expr = true, replace_keycodes = false, desc = 'Go to window above' })
  vim.keymap.set('t', '<M-Right>', [[v:lua.require'utils.term'.running_tui() ? "<M-Right>" : "<Cmd>wincmd l<CR>"]], { expr = true, replace_keycodes = false, desc = 'Go to right window' })

  -- Wisely exit terminal mode with <Esc>
  vim.keymap.set( 't', '<Esc>', [[v:lua.require'utils.term'.running_tui() ? "<Esc>" : "<Cmd>stopi<CR>"]], { expr = true, replace_keycodes = false, desc = 'Exit terminal mode' })

  -- Make `<C-[>` the same as `<Esc>` in terminals with kitty keyboard protocol
  -- support where `<C-[>` and `<Esc>` are treated differently
  vim.keymap.set('t', '<C-[>', '<Esc>', { remap = true })
  -- stylua: ignore end

  vim
    .iter(vim.api.nvim_list_bufs())
    :filter(function(buf)
      return vim.bo[buf].bt == 'terminal'
    end)
    :each(function(buf)
      M.term_init(buf)
    end)

  local groupid = vim.api.nvim_create_augroup('Term', {})
  vim.api.nvim_create_autocmd('TermOpen', {
    group = groupid,
    desc = 'Set terminal keymaps and options, open term in split.',
    callback = function(args)
      M.term_init(args.buf)
    end,
  })
end

return M
