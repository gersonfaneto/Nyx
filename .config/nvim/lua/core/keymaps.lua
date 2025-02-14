vim.g.mapleader = ' '
vim.g.maplocalleader = ','

vim.api.nvim_create_autocmd('UIEnter', {
  once = true,
  callback = function()
    vim.schedule(function()
      local keymaps = {} ---@type table<string, table<string, true>>
      local buf_keymaps = {} ---@type table<integer, table<string, table<string, true>>>

      ---Set keymaps, don't override existing keymaps unless `opts.unique` is false
      ---@param modes string|string[] mode short-name
      ---@param lhs string left-hand side of the mapping
      ---@param rhs string|function right-hand side of the mapping
      ---@param opts? vim.keymap.set.Opts
      ---@return nil
      local function map(modes, lhs, rhs, opts)
        if opts and opts.unique == false then
          vim.keymap.set(modes, lhs, rhs, opts)
          return
        end

        if type(modes) ~= 'table' then
          modes = { modes }
        end

        if not opts or not opts.buffer then -- global keymaps
          for _, mode in ipairs(modes) do
            if not keymaps[mode] then
              keymaps[mode] = {}
              for _, keymap in ipairs(vim.api.nvim_get_keymap(mode)) do
                keymaps[mode][vim.keycode(keymap.lhs)] = true
              end
            end
            if not keymaps[mode][vim.keycode(lhs)] then
              vim.keymap.set(mode, lhs, rhs, opts)
            end
          end
        else -- buffer-local keymaps
          local buf = type(opts.buffer) == 'number' and opts.buffer or 0 --[[@as integer]]
          if not buf_keymaps[buf] then
            buf_keymaps[buf] = {}
          end
          local maps = buf_keymaps[buf]
          for _, mode in ipairs(modes) do
            if not maps[mode] then
              maps[mode] = {}
              for _, keymap in ipairs(vim.api.nvim_buf_get_keymap(0, mode)) do
                maps[mode][vim.keycode(keymap.lhs)] = true
              end
            end
            if not maps[mode][vim.keycode(lhs)] then
              vim.keymap.set(mode, lhs, rhs, opts)
            end
          end
        end
      end

      -- Multi-window operations
      -- stylua: ignore start
      map({ 'x', 'n' }, '<M-w>', '<C-w>w', { desc = 'Cycle through windows' })
      map({ 'x', 'n' }, '<M-W>', '<C-w>W', { desc = 'Cycle through windows reversely' })
      map({ 'x', 'n' }, '<M-H>', '<C-w>H', { desc = 'Move window to far left' })
      map({ 'x', 'n' }, '<M-J>', '<C-w>J', { desc = 'Move winow to very bottom' })
      map({ 'x', 'n' }, '<M-K>', '<C-w>K', { desc = 'Move window to very top' })
      map({ 'x', 'n' }, '<M-L>', '<C-w>L', { desc = 'Move window to far right' })
      map({ 'x', 'n' }, '<M-p>', '<C-w>p', { desc = 'Go to the previous window' })
      map({ 'x', 'n' }, '<M-r>', '<C-w>r', { desc = 'Rotate windows downwords/rightwards' })
      map({ 'x', 'n' }, '<M-R>', '<C-w>r', { desc = 'Rotate windows upwards/leftwords' })
      map({ 'x', 'n' }, '<M-v>', '<C-w>v', { desc = 'Split window vertically' })
      map({ 'x', 'n' }, '<M-s>', '<C-w>s', { desc = 'Split window horizontally' })
      map({ 'x', 'n' }, '<M-x>', '<C-w>x', { desc = 'Exchange current window with next one' })
      map({ 'x', 'n' }, '<M-z>', '<C-w>z', { desc = 'Close preview window' })
      map({ 'x', 'n' }, '<M-c>', '<C-w>c', { desc = 'Close current window' })
      map({ 'x', 'n' }, '<M-q>', '<C-w>q', { desc = 'Quit current window' })
      map({ 'x', 'n' }, '<M-n>', '<C-w>n', { desc = 'Create new window' })
      map({ 'x', 'n' }, '<M-o>', '<C-w>o', { desc = 'Make current window the only one' })
      map({ 'x', 'n' }, '<M-t>', '<C-w>t', { desc = 'Go to the top-left window' })
      map({ 'x', 'n' }, '<M-T>', '<C-w>T', { desc = 'Move window to new tab' })
      map({ 'x', 'n' }, '<M-]>', '<C-w>]', { desc = 'Split and jump to tag under cursor' })
      map({ 'x', 'n' }, '<M-^>', '<C-w>^', { desc = 'Split and edit alternate file' })
      map({ 'x', 'n' }, '<M-b>', '<C-w>b', { desc = 'Go to the bottom-right window' })
      map({ 'x', 'n' }, '<M-d>', '<C-w>d', { desc = 'Split and jump to definition' })
      map({ 'x', 'n' }, '<M-f>', '<C-w>f', { desc = 'Split and edit file under cursor' })
      map({ 'x', 'n' }, '<M-}>', '<C-w>}', { desc = 'Show tag under cursor in preview window' })
      map({ 'x', 'n' }, '<M-g>]', '<C-w>g]', { desc = 'Split and select tag under cursor' })
      map({ 'x', 'n' }, '<M-g>}', '<C-w>g}', { desc = 'Show tag under cursor in preview window' })
      map({ 'x', 'n' }, '<M-g>f', '<C-w>gf', { desc = 'Edit file under cursor in new tab' })
      map({ 'x', 'n' }, '<M-g>F', '<C-w>gF', { desc = 'Edit file under cursor in new tab and jump to line' })
      map({ 'x', 'n' }, '<M-g>t', '<C-w>gt', { desc = 'Go to next tab' })
      map({ 'x', 'n' }, '<M-g>T', '<C-w>gT', { desc = 'Go to previous tab' })
      map({ 'x', 'n' }, '<M-h>', '<C-w><C-h>', { desc = 'Go to the left window' })
      map({ 'x', 'n' }, '<M-j>', '<C-w><C-j>', { desc = 'Go to the window below' })
      map({ 'x', 'n' }, '<M-k>', '<C-w><C-k>', { desc = 'Go to the window above' })
      map({ 'x', 'n' }, '<M-l>', '<C-w><C-l>', { desc = 'Go to the right window' })
      map({ 'x', 'n' }, '<M-Left>', '<C-w><Left>', { desc = 'Go to the left window' })
      map({ 'x', 'n' }, '<M-Down>', '<C-w><Down>', { desc = 'Go to the window below' })
      map({ 'x', 'n' }, '<M-Up>', '<C-w><Up>', { desc = 'Go to the window above' })
      map({ 'x', 'n' }, '<M-Right>', '<C-w><Right>', { desc = 'Go to the right window' })
      map({ 'x', 'n' }, '<M-g><M-]>', '<C-w>g<C-]>', { desc = 'Split and jump to tag under cursor' })
      map({ 'x', 'n' }, '<M-g><Tab>', '<C-w>g<Tab>', { desc = 'Go to last accessed tab' })

      map({ 'x', 'n' }, '<M-=>', '<C-w>=', { desc = 'Make all windows equal size' })
      map({ 'x', 'n' }, '<M-_>', '<C-w>_', { desc = 'Set current window height to maximum' })
      map({ 'x', 'n' }, '<M-|>', '<C-w>|', { desc = 'Set current window width to maximum' })
      -- HACK: The '+' key isn't working properly on my keyboard...
      -- map({ 'x', 'n' }, '<M-+>', 'v:count ? "<C-w>+" : "2<C-w>+"', { expr = true, desc = 'Increase window height' })
      map({ 'x', 'n' }, '<M-S-=>', 'v:count ? "<C-w>+" : "2<C-w>+"', { expr = true, desc = 'Increase window height' })
      map({ 'x', 'n' }, '<M-->', 'v:count ? "<C-w>-" : "2<C-w>-"', { expr = true, desc = 'Decrease window height' })
      map({ 'x', 'n' }, '<M->>', '(v:count ? "" : 4) . (winnr() == winnr("l") ? "<C-w><" : "<C-w>>")', { expr = true, desc = 'Resize window right' })
      map({ 'x', 'n' }, '<M-.>', '(v:count ? "" : 4) . (winnr() == winnr("l") ? "<C-w><" : "<C-w>>")', { expr = true, desc = 'Resize window right' })
      map({ 'x', 'n' }, '<M-<>', '(v:count ? "" : 4) . (winnr() == winnr("l") ? "<C-w>>" : "<C-w><")', { expr = true, desc = 'Resize window left' })
      map({ 'x', 'n' }, '<M-,>', '(v:count ? "" : 4) . (winnr() == winnr("l") ? "<C-w>>" : "<C-w><")', { expr = true, desc = 'Resize window left' })

      map({ 'x', 'n' }, '<C-w>>', '(v:count ? "" : 4) . (winnr() == winnr("l") ? "<C-w><" : "<C-w>>")', { expr = true, desc = 'Resize window right' })
      map({ 'x', 'n' }, '<C-w>.', '(v:count ? "" : 4) . (winnr() == winnr("l") ? "<C-w>>" : "<C-w><")', { expr = true, desc = 'Resize window right' })
      map({ 'x', 'n' }, '<C-w><', '(v:count ? "" : 4) . (winnr() == winnr("l") ? "<C-w>>" : "<C-w><")', { expr = true, desc = 'Resize window left' })
      map({ 'x', 'n' }, '<C-w>,', '(v:count ? "" : 4) . (winnr() == winnr("l") ? "<C-w><" : "<C-w>>")', { expr = true, desc = 'Resize window left' })
      -- HACK: The '+' key isn't working properly on my keyboard...
      -- map({ 'x', 'n' }, '<C-w>+', 'v:count ? "<C-w>+" : "2<C-w>+"', { expr = true, desc = 'Increase window height' })
      map({ 'x', 'n' }, '<C-w><S-=>', 'v:count ? "<C-w>+" : "2<C-w>+"', { expr = true, desc = 'Increase window height' })
      map({ 'x', 'n' }, '<C-w>-', 'v:count ? "<C-w>-" : "2<C-w>-"', { expr = true, desc = 'Decrease window height' })
      -- stylua: ignore end

      -- Wisely exit terminal mode with <Esc>
      map(
        't',
        '<Esc>',
        [[v:lua.require'utils.term'.running_tui() ? "<Esc>" : "<CMD>stopi<CR>"]],
        { expr = true, replace_keycodes = false, desc = 'Exit terminal mode' }
      )

      -- Use <C-\><C-r> to insert contents of a register in terminal mode
      map(
        't',
        [[<C-\><C-r>]],
        [['<C-\><C-n>"' . nr2char(getchar()) . 'pi']],
        { expr = true, desc = 'Insert contents in a register' }
      )

      -- Delete selection in select mode
      map('s', '<BS>', '<C-o>s', { desc = 'Delete selection' })
      map('s', '<C-h>', '<C-o>s', { desc = 'Delete selection' })

      -- More consistent behavior when &wrap is set
      -- stylua: ignore start
      map({ 'n', 'x' }, 'j', 'v:count ? "j" : "gj"', { expr = true, desc = 'Move down' })
      map({ 'n', 'x' }, 'k', 'v:count ? "k" : "gk"', { expr = true, desc = 'Move up' })
      map({ 'n', 'x' }, '<Down>', 'v:count ? "<Down>" : "g<Down>"', { expr = true, replace_keycodes = false, desc = 'Move down' })
      map({ 'n', 'x' }, '<Up>',   'v:count ? "<Up>"   : "g<Up>"',   { expr = true, replace_keycodes = false, desc = 'Move up' })
      map({ 'i' }, '<Down>', '<CMD>norm! g<Down><CR>', { desc = 'Move down' })
      map({ 'i' }, '<Up>',   '<CMD>norm! g<Up><CR>',   { desc = 'Move up' })
      -- stylua: ignore end

      -- Tabpages
      ---@param tab_action function
      ---@param default_count number?
      ---@return function
      local function tabswitch(tab_action, default_count)
        return function()
          local count = default_count or vim.v.count
          local num_tabs = vim.fn.tabpagenr('$')
          if num_tabs >= count then
            tab_action(count ~= 0 and count or nil)
            return
          end
          vim.cmd.tablast()
          for _ = 1, count - num_tabs do
            vim.cmd.tabnew()
          end
        end
      end
      -- stylua: ignore start
      map({ 'n', 'x' }, 'L', tabswitch(vim.cmd.tabnext), { desc = 'Go to next tab' })
      map({ 'n', 'x' }, 'H', tabswitch(vim.cmd.tabprev), { desc = 'Go to previous tab' })
      map({ 'n', 'x' }, 'E', '<CMD>tabnew<CR>', {  desc = 'Open a new tab' })
      map({ 'n', 'x' }, 'R', ':TabRename ', {  desc = 'Rename the current tab' })
      map({ 'n', 'x' }, 'Q', '<CMD>tabclose<CR>', {  desc = 'Close the current tab' })

      -- Correct misspelled word / mark as correct
      map('i', '<C-g>+', '<Esc>[szg`]a', { desc = 'Correct misspelled word before cursor' })
      map('i', '<C-g>=', '<C-g>u<Esc>[s1z=`]a<C-G>u', { desc = 'Add misspelled word before cursor' })
      -- stylua: ignore end

      -- Only clear highlights and message area and don't redraw if search
      -- highlighting is on to avoid flickering
      -- Use `:sil! dif` to suppress error
      -- 'E11: Invalid in command-line window; <CR> executes, CTRL-C quits'
      -- in command window
      --
      -- Don't use `map` here because `<C-l>` is already defined as nvim's
      -- default keymap before loading this config and we want to override
      -- it
      vim.keymap.set(
        { 'n', 'x' },
        '<C-l>',
        [['<CMD>ec|noh|sil! dif<CR>' . (v:hlsearch ? '' : '<C-l>')]],
        {
          expr = true,
          replace_keycodes = false,
          desc = 'Clear and redraw screen',
        }
      )

      -- Edit current file's directory
      map(
        { 'n', 'x' },
        '-',
        [[isdirectory(expand('%:p:h')) ? '<CMD>e%:p:h<CR>' : '<CMD>e ' . fnameescape(getcwd(0)) . '<CR>']],
        {
          expr = true,
          replace_keycodes = false,
          desc = "Edit current file's directory",
        }
      )

      -- Don't include extra spaces around quotes
      -- stylua: ignore start
      map({ 'o', 'x' }, 'a"', '2i"', { noremap = false, desc = 'Select around double quotes' })
      map({ 'o', 'x' }, "a'", "2i'", { noremap = false, desc = 'Select around single quotes' })
      map({ 'o', 'x' }, 'a`', '2i`', { noremap = false, desc = 'Select around backticks' })

      -- Close all floating windows
      map({ 'n', 'x' }, 'q', function() require('utils.misc').close_floats_keymap('q') end, { desc = 'Close all floating windows or start recording macro' })
      map('n', '<Esc>', function() require('utils.misc').close_floats_keymap('<Esc>') end, { desc = 'Close all floating windows' })

      -- Enter insert mode, add a space after the cursor
      map({ 'n', 'x' }, '<M-i>', 'i<Space><Left>', { desc = 'Insert with a space after the cursor' })
      map({ 'n', 'x' }, '<M-I>', 'I<Space><Left>', { desc = 'Insert at start of line or selection with a space after the cursor' })
      map({ 'n', 'x' }, '<M-a>', 'a<Space><Left>', { desc = 'Append with a space after the cursor' })
      map({ 'n', 'x' }, '<M-A>', 'A<Space><Left>', { desc = 'Append at end of line or selection with a space after the cursor' })

      -- Text object: current buffer
      map('x', 'af', ':<C-u>silent! keepjumps normal! ggVG<CR>', { silent = true, noremap = false, desc = 'Select current buffer' })
      map('x', 'if', ':<C-u>silent! keepjumps normal! ggVG<CR>', { silent = true, noremap = false, desc = 'Select current buffer' })
      map('o', 'af', '<CMD>silent! normal m`Vaf<CR><CMD>silent! normal! ``<CR>', { silent = true, noremap = false, desc = 'Select current buffer' })
      map('o', 'if', '<CMD>silent! normal m`Vif<CR><CMD>silent! normal! ``<CR>', { silent = true, noremap = false, desc = 'Select current buffer' })

      map('x', 'iz', [[':<C-u>silent! keepjumps normal! ' . v:lua.require'utils.misc'.textobj_fold('i') . '<CR>']], { silent = true, expr = true, noremap = false, desc = 'Select inside current fold' })
      map('x', 'az', [[':<C-u>silent! keepjumps normal! ' . v:lua.require'utils.misc'.textobj_fold('a') . '<CR>']], { silent = true, expr = true, noremap = false, desc = 'Select around current fold' })
      map('o', 'iz', '<CMD>silent! normal Viz<CR>', { silent = true, noremap = false, desc = 'Select inside current fold' })
      map('o', 'az', '<CMD>silent! normal Vaz<CR>', { silent = true, noremap = false, desc = 'Select around current fold' })

      -- Use 'g{' and 'g}' to go to the first/last line of a paragraph
      map({ 'o' }, 'g{', '<CMD>silent! normal Vg{<CR>', { noremap = false, desc = 'Go to the first line of paragraph' })
      map({ 'o' }, 'g}', '<CMD>silent! normal Vg}<CR>', { noremap = false, desc = 'Go to the last line of paragraph' })
      map({ 'n', 'x' }, 'g{', function() require('utils.misc').goto_paragraph_firstline() end, { noremap = false, desc = 'Go to the first line of paragraph' })
      map({ 'n', 'x' }, 'g}', function() require('utils.misc').goto_paragraph_lastline() end, { noremap = false, desc = 'Go to the last line of paragraph' })
      -- stylua: ignore end

      -- Fzf keymaps
      map('n', '<Leader>.', '<CMD>FZF<CR>', { desc = 'Find files' })
      map('n', '<Leader>ff', '<CMD>FZF<CR>', { desc = 'Find files' })

      map({ 'n', 'x' }, ';', ':', { silent = false, noremap = true })
      map({ 'n', 'x' }, 'q;', 'q:', { silent = false, noremap = true })

      map('n', '<Leader>x', ':.lua<CR>', { desc = 'Run the current line' })
      map('v', '<Leader>x', ':lua<CR>', { desc = 'Run the current selection' })

      map('n', '>', '>>')
      map('n', '<', '<<')
      map('v', '>', '>><Esc>gv')
      map('v', '<', '<<<Esc>gv')

      -- Abbreviations
      map('!a', 'ture', 'true')
      map('!a', 'Ture', 'True')
      map('!a', 'flase', 'false')
      map('!a', 'fasle', 'false')
      map('!a', 'Flase', 'False')
      map('!a', 'Fasle', 'False')
      map('!a', 'lcaol', 'local')
      map('!a', 'lcoal', 'local')
      map('!a', 'locla', 'local')
      map('!a', 'sahre', 'share')
      map('!a', 'saher', 'share')
      map('!a', 'balme', 'blame')
    end)

    return true
  end,
})

vim.api.nvim_create_autocmd('CmdlineEnter', {
  once = true,
  callback = function()
    local key = require('utils.key')
    key.command_map(':', 'lua ')
    key.command_map(';', 'lua ')
    key.command_abbrev('man', 'Man')
    key.command_abbrev('tt', 'tab te')
    key.command_abbrev('bt', 'bot te')
    key.command_abbrev('ht', 'hor te')
    key.command_abbrev('vt', 'vert te')
    key.command_abbrev('rm', '!rm')
    key.command_abbrev('mv', '!mv')
    key.command_abbrev('git', '!git')
    key.command_abbrev('mkd', '!mkdir')
    key.command_abbrev('tree', '!tree')
    key.command_abbrev('mkdir', '!mkdir')
    key.command_abbrev('touch', '!touch')
    return true
  end,
})
