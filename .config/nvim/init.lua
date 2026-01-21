-- Prevent loading lua files from the current directory.
-- E.g. when cwd is `lua/pack/specs/opt/`, `require('nvim-web-devicons')` will
-- load the local `nvim-web-devicons.lua` config file instead of the plugin
-- itself. This will confuse plugins that depends on `nvim-web-devicons`.
-- Subsequent calls to nvim-web-devicons' function may fail.
-- Remove the current directory from `package.path` to avoid errors.
package.path = package.path:gsub('%./%?%.lua;?', '')

-- Enable faster lua loader using byte-compilation
-- https://github.com/neovim/neovim/commit/2257ade3dc2daab5ee12d27807c0b3bcf103cd29
vim.loader.enable()

vim.g.nix_lsp = vim.g.nix_lsp or 'nil'

require('core.opts')
require('core.keymaps')
require('core.autocmds')
require('core.pack')

local load = require('utils.load')

load.on_events('FileType', 'core.treesitter')
load.on_events('DiagnosticChanged', 'core.diagnostic')
load.on_events('FileType', 'core.lsp')

-- stylua: ignore start
vim.keymap.set("n", "<Leader>m", "<Plug>Zoom", { desc = "Toggle zoom", remap = true })

vim.keymap.set('n', '<Leader>eb', function()
  local current = vim.api.nvim_get_option_value('background', {})
  local next = current == 'dark' and 'light' or 'dark'
  vim.api.nvim_set_option_value('background', next, {})
end, { desc = "Toggle background" })

vim.keymap.set("v", "<", "<gv", { desc = "Indent left and reselect" })
vim.keymap.set("v", ">", ">gv", { desc = "Indent right and reselect" })

vim.keymap.set("v", ".", ":normal .<cr>", { desc = "Repeat last command" })

-- Move between panes or create new panes
vim.keymap.set("n", "<M-h>", "<Plug>WinMoveLeft", { desc = "Move to left window", remap = true, silent = true })
vim.keymap.set("n", "<M-j>", "<Plug>WinMoveDown", { desc = "Move to window below", remap = true, silent = true })
vim.keymap.set("n", "<M-k>", "<Plug>WinMoveUp", { desc = "Move to window above", remap = true, silent = true })
vim.keymap.set("n", "<M-l>", "<Plug>WinMoveRight", { desc = "Move to right window", remap = true, silent = true })

-- Quickfix navigation using vim-unimpaired style
vim.keymap.set("n", "[q", "<cmd>cprev<cr>zz", { desc = "Previous quickfix item" })
vim.keymap.set("n", "]q", "<cmd>cnext<cr>zz", { desc = "Next quickfix item" })

-- Move lines with simple arrow keys (can be held down)
vim.keymap.set("n", "<Up>", "<cmd>m .-2<cr>==", { desc = "Move line up" })
vim.keymap.set("n", "<Down>", "<cmd>m .+1<cr>==", { desc = "Move line down" })
vim.keymap.set("v", "<Up>", ":m '<-2<cr>gv=gv", { desc = "Move selection up", silent = true })
vim.keymap.set("v", "<Down>", ":m '>+1<cr>gv=gv", { desc = "Move selection down", silent = true })

-- wrap visual selection in provided wrapper
vim.keymap.set("v", "$(", "<esc>`>a)<esc>`<i(<esc>", { desc = "Wrap in parentheses" })
vim.keymap.set("v", "$[", "<esc>`>a]<esc>`<i[<esc>", { desc = "Wrap in brackets" })
vim.keymap.set("v", "${", "<esc>`>a}<esc>`<i{<esc>", { desc = "Wrap in braces" })
vim.keymap.set("v", "$'", "<esc>`>a'<esc>`<i'<esc>", { desc = "Wrap in single quotes" })
vim.keymap.set("v", '$"', '<esc>`>a"<esc>`<i"<esc>', { desc = "Wrap in double quotes" })
vim.keymap.set("v", "$\\", "<esc>`>o*/<esc>`<O/*<esc>", { desc = "Wrap in C-style comment" })
vim.keymap.set("v", "$<", "<esc>`>a><esc>`<i<<esc>", { desc = "Wrap in angle brackets" })

-- moving up and down work as you would expect
vim.keymap.set("n", "j", 'v:count == 0 ? "gj" : "j"', { expr = true, desc = "Move down (wrap friendly)" })
vim.keymap.set("n", "k", 'v:count == 0 ? "gk" : "k"', { expr = true, desc = "Move up (wrap friendly)" })
vim.keymap.set("n", "^", 'v:count == 0 ? "g^" :  "^"', { expr = true, desc = "Start of line (wrap friendly)" })
vim.keymap.set("n", "$", 'v:count == 0 ? "g$" : "$"', { expr = true, desc = "End of line (wrap friendly)" })

-- custom text objects
-- inner-line
vim.keymap.set("x", "il", ":<c-u>normal! g_v^<cr>", { desc = "Inner line text object" })
vim.keymap.set("o", "il", ":<c-u>normal! g_v^<cr>", { desc = "Inner line text object" })
-- around line
vim.keymap.set("v", "al", ":<c-u>normal! $v0<cr>", { desc = "Around line text object" })
vim.keymap.set("o", "al", ":<c-u>normal! $v0<cr>", { desc = "Around line text object" })

-- interesting word mappings
vim.keymap.set("n", "<leader>0", "<Plug>ClearInterestingWord", { desc = "Clear interesting word", remap = true })
vim.keymap.set("n", "<leader>1", "<Plug>HiInterestingWord1", { desc = "Highlight interesting word 1", remap = true })
vim.keymap.set("n", "<leader>2", "<Plug>HiInterestingWord2", { desc = "Highlight interesting word 2", remap = true })
vim.keymap.set("n", "<leader>3", "<Plug>HiInterestingWord3", { desc = "Highlight interesting word 3", remap = true })
vim.keymap.set("n", "<leader>4", "<Plug>HiInterestingWord4", { desc = "Highlight interesting word 4", remap = true })
vim.keymap.set("n", "<leader>5", "<Plug>HiInterestingWord5", { desc = "Highlight interesting word 5", remap = true })
vim.keymap.set("n", "<leader>6", "<Plug>HiInterestingWord6", { desc = "Highlight interesting word 6", remap = true })

-- Jump to start and end of line using the home row keys
vim.keymap.set('', 'H', '^')
vim.keymap.set('', 'L', '$')

-- <CR><CR> toggles between buffers
-- vim.keymap.set('n', '<CR><CR>', '<C-^>')

-- "very magic" (less escaping needed) regexes by default
vim.keymap.set('n', '?', '?\\v')
vim.keymap.set('n', '/', '/\\v')
vim.keymap.set('c', '%s/', '%sm/')

-- let the left and right arrows be useful: they can switch buffers
vim.keymap.set('n', '<Left>', ':bp<cr>')
vim.keymap.set('n', '<Right>', ':bn<cr>')
-- stylua: ignore end

vim.keymap.set('v', '<leader>y', function()
  local mode = vim.fn.mode()

  if mode ~= 'v' and mode ~= 'V' then
    return
  end

  vim.cmd([[silent normal! "xy]])
  local text = vim.fn.getreg('x')
  ---@diagnostic disable-next-line: param-type-mismatch
  local lines = vim.split(text, '\n', { plain = true })

  local converted = {}
  for _, line in ipairs(lines) do
    local l = line:gsub('\t', ' ')
    table.insert(converted, l)
  end

  local min_indent = math.huge
  for _, line in ipairs(converted) do
    if line:match('[^%s]') then
      local indent = #(line:match('^%s*'))
      min_indent = math.min(min_indent, indent)
    end
  end

  min_indent = min_indent == math.huge and 0 or min_indent

  local result = {}
  for _, line in ipairs(converted) do
    if line:match('^%s*$') then
      table.insert(result, '')
    else
      local processed = line:sub(min_indent + 1)
      processed = processed:gsub('^%s+', function(spaces)
        return string.rep('  ', math.floor(#spaces / 2))
      end)
      table.insert(result, processed)
    end
  end

  local normalized = table.concat(result, '\n')
  vim.fn.setreg('+', normalized)
  vim.notify('Copied normalized text to clipboard')
end, { desc = 'Copy and normalize' })

vim.api.nvim_create_user_command('R', function(opts)
  -- Expand % and # BEFORE opening new buffer
  local current = vim.fn.expand('%:p')
  local alt = vim.fn.expand('#:p')
  local cmd =
    opts.args:gsub('%%:p', current):gsub('%%', current):gsub('#', alt)
  vim.cmd('new')
  vim.bo.buftype = 'nofile'
  vim.bo.bufhidden = 'hide'
  vim.bo.swapfile = false
  vim.b.no_auto_close = true
  vim.fn.termopen(cmd, {
    on_stdout = function()
      vim.schedule(function()
        vim.cmd([[ stopinsert ]])
        vim.api.nvim_feedkeys('G', 't', false)
      end)
    end,
  })
  -- stylua: ignore start
  vim.api.nvim_buf_set_keymap( 0, 'n', 'q', ':q!<CR>', { noremap = true, silent = true })
  -- stylua: ignore end
end, { nargs = '+', complete = 'shellcmdline' })

vim.keymap.set('n', '<leader>if', '<cmd>source %<cr>')
vim.keymap.set({ 'n', 'x' }, '<leader>ix', ':.lua<cr>')

package.loaded['compile-mode'] = nil

require('compile-mode').reset()

local compile = require('compile-mode')

-- stylua: ignore start
vim.keymap.set('n', '<leader>Cc', compile.command, { desc = 'Run a command' })
vim.keymap.set('n', '<leader>Cr', compile.recompile, { desc = 'Re-run last command' })
vim.keymap.set('n', '<leader>Cb', compile.scroll_to_bottom, { desc = 'Scroll compilation buffer to bottom' })
vim.keymap.set('n', '<leader>Ct', compile.toggle_window, { desc = 'Toggle compilation buffer' })
vim.keymap.set('n', '<leader>CR', compile.reset, { desc = 'Reset' })
-- stylua: ignore end

vim.api.nvim_create_autocmd('BufEnter', {
  command = [[set formatoptions-=cro]],
})
