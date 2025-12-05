vim.b.minisurround_config = {
  custom_surroundings = {
    -- Markdown link
    -- `ysL` + [type/paste link] + <CR> - add link
    -- `dsL` - delete link
    L = {
      input = { '%[().-()%]%(.-%)' },
      output = function()
        local link = require('mini.surround').user_input('Link')
        return { left = '[', right = '](' .. link .. ')' }
      end,
    },
    -- B for bold
    B = {
      input = { '%[().-()%]%(.-%)' },
      output = { left = '**', right = '**' },
    },
    -- I for italic
    I = {
      input = { '%[().-()%]%(.-%)' },
      output = { left = '_', right = '_' },
    },
    -- E for code
    E = {
      input = { '%[().-()%]%(.-%)' },
      output = function()
        local char = vim.fn.visualmode() == 'V' and '\n```\n' or '`'
        return { left = char, right = char }
      end,
    },
  },
}

-- stylua: ignore start
vim.keymap.set('v', '<C-k>', 'ysL', { buffer = 0, desc = 'Add link', remap = true })
vim.keymap.set('v', '<C-b>', 'ysB', { buffer = 0, desc = 'Add bold', remap = true })
vim.keymap.set('v', '<C-i>', 'ysI', { buffer = 0, desc = 'Add italic', remap = true })
vim.keymap.set('v', '<C-e>', 'ysE', { buffer = 0, desc = 'Add code', remap = true })
-- stylua: ignore end
