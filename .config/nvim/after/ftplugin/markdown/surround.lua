vim.b.minisurround_config = {
  custom_surroundings = {
    -- L for link
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
vim.keymap.set('v', '<C-l>', 'saL', { buffer = 0, desc = 'Add link', remap = true })
vim.keymap.set('v', '<C-b>', 'saB', { buffer = 0, desc = 'Add bold', remap = true })
vim.keymap.set('v', '<C-i>', 'saI', { buffer = 0, desc = 'Add italic', remap = true })
vim.keymap.set('v', '<C-e>', 'saE', { buffer = 0, desc = 'Add code', remap = true })
-- stylua: ignore end
